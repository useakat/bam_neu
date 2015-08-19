      real*8 function hfunc1D(x,z)
      implicit none
      include 'inc/const.inc'
      include 'inc/params.inc'
      include 'inc/minfunc.inc'
      integer i
      integer Ediv,ierr,oct
      real*8 x,z(zdim),E,YY,fxsec_CCQE,fxsec_CCRes,fxsec_NCpi0
      real*8 s212_2,s223_2,s213_2,hdm21_2,hdm31_2,hdCP,hhdCP
      real*8 xmin,xmax,s2atm2,Enumin,Enumax,Enu,nuevent,ff
      real*8 hrho,xsec,fxsec,hfCCQEn,hfCCQEa,hfCCResn,hfCCResa,hfpi0
      real*8 fD,frho,fnmn,fnen,fnma,fnea,famn,faen,fama,faea
      real*8 frac,frac1,frac2,frac3,frac4,corr_fact
      real*8 flux_beam,xsec_NC,xsec_CC,xsec_CCQE,prob
      real*8 prob_approx1,prob_approx2,smear_CCQE_mu
      external flux_beam,xsec_NC,xsec_CC,xsec_CCQE,prob
      external prob_approx1,prob_approx2,smear_CCQE_mu      

      if (iinput.eq.1) then
         if (ithatm.eq.1) then
            call convert_th(z(1),z(2),z(3),s212_2,s223_2,s213_2)
            oct = -1
         elseif (ithatm.eq.2) then
            s2atm2 = 4*z(2)*(1d0 -z(2)) 
            call convert_th(z(1),s2atm2,z(3),s212_2,s223_2,s213_2)
            if (z(2).le.0.5d0) then
               oct = -1
            elseif (z(2).gt.0.5d0) then
               oct = 1
            endif
         else
            write(*,*) "ERROR:hfunc1D:Invalid ithatm (1/2)"
            write(*,*) "Stopping program..."
            stop
         endif
      elseif (iinput.eq.2) then
         s212_2 = z(1)
         if (ithatm.eq.1) then
            s223_2 = z(2)
            oct = -1
         elseif (ithatm.eq.2) then
            s223_2 = 4*z(2)*(1d0 -z(2))
            if (z(2).le.0.5d0) then
               oct = -1
            elseif (z(2).gt.0.5d0) then
               oct = 1
            endif
         else
            write(*,*) "ERROR:hfunc1D:Invalid ithatm (1/2)"
            write(*,*) "Stopping program..."
            stop
         endif
         s213_2 = z(3)
      else
         write(*,*) "ERROR:hfunc1D:Invalid iinput (1/2)"
         write(*,*) "Stopping program..."
         stop
      endif
c      if (s223_2-1d0.gt.1d-7) then
c         hfunc1D = 0d0
c         return
c      endif
      hdm21_2 = z(4)
      hdm31_2 = MHH*z(5)
      hdCP = z(6)/360d0*2*pi ! convert degree to radian
      hfCCQEn = z(7)
      hfCCQEa = z(8)
      hfCCResn = z(51)
      hfCCResa = z(52)
      hfpi0 = z(53)
      if (iD.eq.1) then
         fD = z(9)
         frho = z(10)
         fnmn = z(11)
         fnen = z(12)
         fnma = z(13)
         fnea = z(14)
         famn = z(15)
         faen = z(16)
         fama = z(17)
         faea = z(18)
      elseif (iD.eq.2) then
         fD = z(23)
         frho = z(24)
         fnmn = z(25)
         fnen = z(26)
         fnma = z(27)
         fnea = z(28)
         famn = z(29)
         faen = z(30)
         fama = z(31)
         faea = z(32)
      elseif (iD.eq.3) then
         fD = z(37)
         frho = z(38)
c$$$         fnmn = z(11)
c$$$         fnen = z(12)
c$$$         fnma = z(13)
c$$$         fnea = z(14)
c$$$         famn = z(15)
c$$$         faen = z(16)
c$$$         fama = z(17)
c$$$         faea = z(18)
         fnmn = z(39)
         fnen = z(40)
         fnma = z(41)
         fnea = z(42)
         famn = z(43)
         faen = z(44)
         fama = z(45)
         faea = z(46)
      endif      
      if (detect.gt.0) then
         fxsec_CCQE = hfCCQEn
         fxsec_CCRes = hfCCResn
      elseif (detect.lt.0) then
         fxsec_CCQE = hfCCQEa
         fxsec_CCRes = hfCCResa
      endif
      fxsec_NCpi0 = hfpi0
      if (beam.gt.0) then
         if (nu_mode.eq.1) ff = fnen
         if (nu_mode.eq.-1) ff = fnea
         if (nu_mode.eq.2) ff = fnmn
         if (nu_mode.eq.-2) ff = fnma
      elseif (beam.lt.0) then
         if (nu_mode.eq.1) ff = faen
         if (nu_mode.eq.-1) ff = faea
         if (nu_mode.eq.2) ff = famn
         if (nu_mode.eq.-2) ff = fama
      endif
      
      if ((r_nu.ge.0).and.(r_anu.ge.0)) then
         if ((r_nu.gt.0).or.(r_anu.gt.0)) then
            if (beam.eq.1) then
               YY = Y*r_nu/(r_nu + r_anu)
            elseif (beam.eq.-1) then
               YY = Y*r_anu/(r_nu + r_anu)
            endif
         else
            write(*,*) "ERROR: r_nu, r_anu = 0"
         endif
      else
         write(*,*) "ERROR: r_nu or r_anu are negative"
      endif

      E = x
      if (icc.eq.1) then
C     Cross sections
         if (ismear.eq.0) then
            frac = 0d0
            if (iCCQE.eq.1) then
               call get_xsecfrac3(E,icc,1,1,detect,frac1)
               call get_xsecfrac3(E,icc,1,2,detect,frac2)
               frac = frac +fxsec_CCQE*(frac1 +frac2)
            endif
            if (iCCRes.eq.1) then
               call get_xsecfrac3(E,icc,1,3,detect,frac3)
               call get_xsecfrac3(E,icc,1,4,detect,frac4)
               frac = frac +fxsec_CCRes*(frac3 +frac4)
            endif
c            xsec = xsec_CC(detect,E)*frac
c            xsec = fxsec_CCQE*xsec_CCQE(detect,E)
            xsec = xsec_CCQE(detect,E)
         elseif (ismear.eq.1) then
            xsec = xsec_CC(detect,E)
         endif
      elseif (icc.eq.2) then
         xsec = fxsec_NCpi0*xsec_NC(detect,E)
      endif
c      xsec_CCQE(detect,E)/(100*1d9*avog)
c      xsec = xsec_CCQE(detect,E)
c      xsec = xsec_CC(detect,E)/(33.6d30*8) ! nu_e-H2O xsec/neutron
c      xsec = xsec_CC(detect,E)/(33.6d30*10) ! bar{nu}_e-H2O xsec/proton
c      xsec = xsec_CC(detect,E)/(3.34d31*8)*4.89d33/18d0 ! nu_e-H2O xsec/neutron scaled to 1kton LS
c      xsec = xsec_CC(detect,E)/(3.34d31*10)*6.24d33/18d0 ! bar{nu}_e-H2O xsec/proton scaled to 1kton LS
      hrho = frho*rho
      if (ihfunc.eq.0) then
         hfunc1D = ff*flux_beam(beam,oab,nu_mode,E,L)*xsec*fD*V*YY
     &        *prob(nu_mode,detect,E,L,s212_2,s223_2,s213_2,hdm21_2
     &        ,hdm31_2,hdCP,hrho,oct)
      elseif (ihfunc.eq.1) then
         hfunc1D = flux_beam(beam,oab,nu_mode,E,L)
      elseif (ihfunc.eq.2) then
         hfunc1D = prob(nu_mode,detect,E,L,s212_2,s223_2,s213_2,hdm21_2
     &        ,hdm31_2,hdCP,hrho,oct)
      elseif (ihfunc.eq.3) then
         hfunc1D = xsec
      elseif (ihfunc.eq.4) then
         hfunc1D = flux_beam(beam,oab,nu_mode,E,L)*xsec*V*YY
     &        *prob(nu_mode,detect,E,L,s212_2,s223_2,s213_2,hdm21_2
     &        ,hdm31_2,hdCP,hrho,oct)
      endif

      return
      end
