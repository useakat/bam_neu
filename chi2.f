      real*8 function chi2(nout,dat,th,nbins,nparm,parm,parm0,error)
C     ****************************************************
C     By Yoshitaro Takaesu @KIAS AUG 27 2012
C
C     Calculate chi^2 from two real*8 arrays
C     ****************************************************
      implicit none

C     ARGUMENTS 
      integer nbins,nmin,nout,nparm,minevents
      parameter (minevents=0)
      real*8 dat(nbins),th(nbins),parm(nparm),parm0(nparm),error(nparm)
C     LOCAL VARIABLES 
      integer i
      real*8 chi,sgm2,chi22
C     ----------
C     BEGIN CODE
C     ----------
      chi2 = 0d0
      do i = 1,nbins
         if (dat(i).ge.minevents) then
            sgm2 = dat(i)
            chi = ( dat(i) -th(i) )**2 / sgm2
            chi2 = chi2 + chi
         elseif (dat(i).ge.0) then
c            write(nout,*) "ERROR: events in ",i,"th bin is too small. " 
c     &           ,"Please reconsider the bin size."
c            stop
         else
            write(nout,*) "ERROR: events in ",i," th bin is negative."
            stop
         endif
      enddo

      chi22 = 0d0
      do i = 1,nparm
         chi22 = chi22 +( parm(i) -parm0(i) )**2 / error(i)**2
      enddo
      chi2 = chi2 +chi22
      

      return
      end


      real*8 function chi2_2(nout,dat,th,x,nbins,nparm,parm,parm0,error)
C     ****************************************************
C     By Yoshitaro Takaesu @KIAS AUG 27 2012
C
C     Calculate chi^2 from two real*8 arrays
C     ****************************************************
      implicit none

C     ARGUMENTS 
      integer nbins,nmin,nout,nparm,minevents
      parameter (minevents=0)
      real*8 dat(nbins),th(nbins),parm(nparm),parm0(nparm),error(nparm)
      real*8 x(0:20000)
C     LOCAL VARIABLES 
      integer i,nn,ipos
      real*8 sgm2,chi22,sumdat,sumth
      integer ifirst
      real*8 final_bins
      common /first/ final_bins,ifirst
C     ----------
C     BEGIN CODE
C     ----------
      nn = 1

      final_bins = 0
      chi2_2 = 0d0
      ipos = 0
      sumdat = 0d0
      sumth = 0d0
      open(41,file="final_bining_dat.dat",status="replace")
      open(42,file="final_bining_th.dat",status="replace")
      open(43,file="final_bining_dchi2.dat",status="replace")
      do i = 1,nbins
         ipos = ipos +1
         sumdat = sumdat +dat(i)
         sumth = sumth +th(i)
         if (ipos.eq.nn) then
            if (sumdat.ge.minevents) then
               write(41,*) x(i),sumdat
               write(42,*) x(i),sumth
               final_bins = final_bins + 1
               sgm2 = sumdat
               chi2_2 = chi2_2 + ( sumdat -sumth )**2 / sgm2
               write(43,*) x(i),( sumdat -sumth )**2 / sgm2
               ipos = 0
               sumdat = 0d0
               sumth = 0d0
            elseif (sumdat.ge.0) then
               ipos = ipos -1
c               write(nout,*) "Events in ",i,"th bin is less than "
c     &              ,minevents,"."
            else
               write(nout,*) "ERROR: events in ",i
     &              ," th bin is negative."
c               stop
            endif
         endif
      enddo
      close(43)
      close(42)
      close(41)
      
      chi22 = 0d0
      do i = 1,nparm
         chi22 = chi22 +( parm(i) -parm0(i) )**2 / error(i)**2
      enddo
      chi2_2 = chi2_2 +chi22
      

      return
      end

      real*8 function chi2_stat(dat,th,nbins,x,outnbins,outx)
C     ****************************************************
C     By Yoshitaro Takaesu @KIAS AUG 27 2012
C
C     Calculate chi^2_stat from two real*8 arrays
C     ****************************************************
      implicit none
C     ARGUMENTS 
      integer nbins,outnbins
      real*8 dat(nbins),th(nbins)
      real*8 x(0:nbins),outx(0:nbins)
C     LOCAL VARIABLES 
      integer i,nn,ipos,minevents
      parameter (minevents=1)
      real*8 sgm2,chi22,sumdat,sumth
c      integer ifirst
c      real*8 final_bins
c      common /first/ final_bins,ifirst
C     ----------
C     BEGIN CODE
C     ----------
      nn = 1
      outnbins = 0
      chi2_stat = 0d0
      ipos = 0
      sumdat = 0d0
      sumth = 0d0
      outx(0) = x(0)
      do i = 1,nbins
         ipos = ipos +1
         sumdat = sumdat +dat(i)
         sumth = sumth +th(i)
         if (ipos.eq.nn) then
            if (sumdat.ge.minevents) then
               sgm2 = sumdat
               chi2_stat = chi2_stat + ( sumdat -sumth )**2 / sgm2
               outnbins = outnbins + 1
               outx(outnbins) = x(i) 
               ipos = 0
               sumdat = 0d0
               sumth = 0d0
            elseif (sumdat.ge.0) then
               ipos = ipos -1
            else
               write(*,*) "ERROR: events in ",i
     &              ," th bin is negative."
               write(*,*) dat(i)
            endif
         endif
      enddo
           
      return
      end


      subroutine get_chi2_stat_bin(dat,th,nbins,x,chi2bin,chi2sum
     &     ,outnbins,outx)
C     ****************************************************
C     By Yoshitaro Takaesu @KIAS AUG 27 2012
C
C     Calculate chi^2_stat in each bin from two real*8 arrays
C     ****************************************************
      implicit none
C     ARGUMENTS 
      integer nbins,outnbins
      real*8 dat(nbins),th(nbins),chi2sum
      real*8 x(0:nbins),outx(0:nbins),chi2bin(nbins)
C     LOCAL VARIABLES 
      integer i,nn,ipos,minevents,ibin
      parameter (minevents=1)
      real*8 sgm2,chi22,sumdat,sumth
c      integer ifirst
c      real*8 final_bins
c      common /first/ final_bins,ifirst
C     ----------
C     BEGIN CODE
C     ----------
      nn = 1
      chi2sum = 0d0
      ipos = 0
      sumdat = 0d0
      sumth = 0d0
      outnbins = 0
      outx(0) = x(0)
      do i = 1,nbins
         ipos = ipos +1
         sumdat = sumdat +dat(i)
         sumth = sumth +th(i)
         if (ipos.eq.nn) then
            if (sumdat.ge.minevents) then
               outnbins = outnbins + 1
               outx(outnbins) = x(i) 
               sgm2 = sumdat
               chi2bin(outnbins) = ( sumdat -sumth )**2 / sgm2
               chi2sum = chi2sum +chi2bin(outnbins)
               ipos = 0
               sumdat = 0d0
               sumth = 0d0
            elseif (sumdat.ge.0) then
               ipos = ipos -1
            else
               write(*,*) "ERROR: events in ",i
     &              ," th bin is negative."
               write(*,*) dat(i)
            endif
         endif
      enddo
           
      return
      end


      real*8 function chi2_para(nparam,param,param0,error,flag)
C     ****************************************************
C     By Yoshitaro Takaesu @KIAS MAY 27 2013
C
C     Calculate chi^2_para from two real*8 arrays
C     ****************************************************
      implicit none
C     ARGUMENTS 
      integer nparam,flag(nparam)
      real*8 param(nparam),param0(nparam)
      real*8 error(nparam)
C     LOCAL VARIABLES 
      integer i
C     ----------
C     BEGIN CODE
C     ----------
      chi2_para = 0d0
      do i = 1,nparam
         if (flag(i).eq.1) then
            chi2_para = chi2_para +( param(i) -param0(i) )**2
     &           /error(i)**2
         endif
      enddo

      return
      end
