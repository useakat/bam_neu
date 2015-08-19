      subroutine display_results
C     ****************************************************
C     By Yoshitaro Takaesu @KIAS JAN 7 2014
C     ****************************************************
      implicit none
C     GLOBAL VARIABLES
      include 'inc/params.inc'
      include 'inc/main.inc'
      real*8 nevent_pi0bg_dat(ndetect),nevent_total_dat
      common /nevent/ nevent_pi0bg_dat,nevent_total_dat
C     CONSTANTS
C     ARGUMENTS
      integer iD
C     LOCAL VARIABLES 
      integer i
      real*8 nevent_tmp
      real*8 hErec_total_dat(maxnbin)
C     EXTERNAL FUNCTIONS
C     ----------
C     BEGIN CODE
C     ----------
      if (mode.eq.1) then
         write(*,*) ""
         write(*,*) ""
         write(*,*) "Delta Chi^2:",dchisqmin
         write(*,*) "Sensitivity:",sensitivity
      endif
      write(*,*) ""
      write(*,*) "Event Numbers"
      write(*,*)
      write(*,*) "nu_mu beam"
      if (iSK.eq.1) then
         write(*,*) " SK"
      endif
      write(*,*) "  e events:",nevent_e(1,1)
      write(*,*) "  e-like events:",nevent_elike(1,1)
      write(*,*) "   e > e:",nevent_e2e(1,1)
      write(*,*) "   mu > e:",nevent_mu2e(1,1)
      write(*,*) "   NC pi0:",nevent_pi0(1,1)
      write(*,*) "   gamma:",nevent_gam(1,1)
      write(*,*) "  mu events:",nevent_mu(1,1)
      write(*,*) "  mu-like events:",nevent_mulike(1,1)
      write(*,*) "   mu > mu:",nevent_mu2mu(1,1)
      write(*,*) "   e > mu:",nevent_e2mu(1,1)
      if (iOki.eq.1) then
         write(*,*) " Oki"
         write(*,*) "  e events:",nevent_e(2,1)
         write(*,*) "  e-like events:",nevent_elike(2,1)
         write(*,*) "   e > e:",nevent_e2e(2,1)
         write(*,*) "   mu > e:",nevent_mu2e(2,1)
         write(*,*) "   NC pi0:",nevent_pi0(2,1)
         write(*,*) "   gamma:",nevent_gam(2,1)
         write(*,*) "  mu events:",nevent_mu(2,1)
         write(*,*) "  mu-like events:",nevent_mulike(2,1)
         write(*,*) "   mu > mu:",nevent_mu2mu(2,1)
         write(*,*) "   e > mu:",nevent_e2mu(2,1)
      endif
      if (iKr.eq.1) then
         write(*,*) " Korea"
         write(*,*) "  e events:",nevent_e(3,1)
         write(*,*) "  e-like events:",nevent_elike(3,1)
         write(*,*) "   e > e:",nevent_e2e(3,1)
         write(*,*) "   mu > e:",nevent_mu2e(3,1)
         write(*,*) "   NC pi0:",nevent_pi0(3,1)
         write(*,*) "   gamma:",nevent_gam(3,1)
         write(*,*) "  mu events:",nevent_mu(3,1)
         write(*,*) "  mu-like events:",nevent_mulike(3,1)
         write(*,*) "   mu > mu:",nevent_mu2mu(3,1)
         write(*,*) "   e > mu:",nevent_e2mu(3,1)
      endif
      write(*,*) ""
      write(*,*) "anti-nu_mu beam"
      if (iSK.eq.1) then
         write(*,*) " SK"
      endif
      write(*,*) "  e events:",nevent_e(1,-1)
      write(*,*) "  e-like events:",nevent_elike(1,-1)
      write(*,*) "   e > e:",nevent_e2e(1,-1)
      write(*,*) "   mu > e:",nevent_mu2e(1,-1)
      write(*,*) "   NC pi0:",nevent_pi0(1,-1)
      write(*,*) "   gamma:",nevent_gam(1,-1)
      write(*,*) "  mu events:",nevent_mu(1,-1)
      write(*,*) "  mu-like events:",nevent_mulike(1,-1)
      write(*,*) "   mu > mu:",nevent_mu2mu(1,-1)
      write(*,*) "   e > mu:",nevent_e2mu(1,-1)
      if (iOki.eq.1) then
         write(*,*) " Oki"
         write(*,*) "  e events:",nevent_e(2,-1)
         write(*,*) "  e-like events:",nevent_elike(2,-1)
         write(*,*) "   e > e:",nevent_e2e(2,-1)
         write(*,*) "   mu > e:",nevent_mu2e(2,-1)
         write(*,*) "   NC pi0:",nevent_pi0(2,-1)
         write(*,*) "   gamma:",nevent_gam(2,-1)
         write(*,*) "  mu events:",nevent_mu(2,-1)
         write(*,*) "  mu-like events:",nevent_mulike(2,-1)
         write(*,*) "   mu > mu:",nevent_mu2mu(2,-1)
         write(*,*) "   e > mu:",nevent_e2mu(2,-1)
      endif
      if (iKr.eq.1) then
         write(*,*) " Korea"
         write(*,*) "  e events:",nevent_e(3,-1)
         write(*,*) "  e-like events:",nevent_elike(3,-1)
         write(*,*) "   e > e:",nevent_e2e(3,-1)
         write(*,*) "   mu > e:",nevent_mu2e(3,-1)
         write(*,*) "   NC pi0:",nevent_pi0(3,-1)
         write(*,*) "   gamma:",nevent_gam(3,-1)
         write(*,*) "  mu events:",nevent_mu(3,-1)
         write(*,*) "  mu-like events:",nevent_mulike(3,-1)
         write(*,*) "   mu > mu:",nevent_mu2mu(3,-1)
         write(*,*) "   e > mu:",nevent_e2mu(3,-1)
      endif
      write(*,*) ""

      return
      end
