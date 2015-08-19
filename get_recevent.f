      subroutine get_recevent(z,nbins,hErec_raw,hErec_rec)
C     ****************************************************
C     By Yoshitaro Takaesu @KIAS JAN 7 2014
C     ****************************************************
      implicit none
C     GLOBAL VARIABLES
      include 'inc/params.inc'
C     CONSTANTS
C     ARGUMENTS 
      integer nbins
      real*8 z(zdim)
      real*8 hErec_raw(maxnbin,imaxint,nmode,-3:3,-3:3,ndetect,-1:1)
      real*8 hErec_rec(maxnbin,imaxint,nmode_rec,-3:3,-3:3,ndetect,-1:1)
C     LOCAL VARIABLES 
      integer i,j,k,l1,l2,l3
      real*8 effe,effmu,Pe2m,Pm2e
C     EXTERNAL FUNCTIONS
C     ----------
C     BEGIN CODE
C     ----------
      do l2 = -1,1,2
         do l1 = 1,ndetect
            if (l1.eq.1) then  ! SK
               effe = z(19)
               effmu = z(20)
               Pe2m = z(21)
               Pm2e = z(22)
            elseif (l1.eq.2) then  ! Oki
               effe = z(33)
               effmu = z(34)
               Pe2m = z(35)
               Pm2e = z(36)
            elseif (l1.eq.3) then  ! Korea
               effe = z(47)
               effmu = z(48)
               Pe2m = z(49)
               Pm2e = z(50)
            endif
            if (effe.gt.1d0) effe = 1d0
            if (effe.lt.0d0) effe = 0d0
            if (effmu.gt.1d0) effmu = 1d0
            if (effmu.lt.0d0) effmu = 0d0
            if (Pe2m.gt.1d0) Pe2m = 1d0
            if (Pe2m.lt.0d0) Pe2m = 0d0               
            if (Pm2e.gt.1d0) Pm2e = 1d0
            if (Pm2e.lt.0d0) Pm2e = 0d0               

            do k = -3,3
               do j = -3,3
                  do l3 = 1,imaxint
                     do i = 1,nbins
C     e > e events
                        hErec_rec(i,l3,1,j,k,l1,l2) = 
     &                       effe*(1d0 -Pe2m)*hErec_raw(i,l3,1,j,k,l1
     &                       ,l2)
C     mu > mu  events
                        hErec_rec(i,l3,2,j,k,l1,l2) = 
     &                       effmu*(1d0 -Pm2e)*hErec_raw(i,l3,2,j,k,l1
     &                       ,l2)
C     Just for bokkkeeping of NC-pi0 and NC-gamma events
                        hErec_rec(i,l3,3,j,k,l1,l2) = hErec_raw(i,l3,3,j
     &                       ,k,l1,l2)
                        hErec_rec(i,l3,4,j,k,l1,l2) = hErec_raw(i,l3,4,j
     &                       ,k,l1,l2)
C     mu > e miss-ID events
                        hErec_rec(i,l3,5,j,k,l1,l2) = 
     &                       effmu*Pm2e*hErec_raw(i,l3,2,j,k,l1,l2)
C     e > mu miss-ID events
                        hErec_rec(i,l3,6,j,k,l1,l2) = 
     &                       effe*Pe2m*hErec_raw(i,l3,1,j,k,l1,l2)
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo

      return
      end


      subroutine get_recevent_re(z,nbins,hErec_raw,hErec_rec)
C     ****************************************************
C     By Yoshitaro Takaesu @KIAS Feb 24 2014
C     ****************************************************
      implicit none
C     GLOBAL VARIABLES
      include 'inc/params.inc'
C     CONSTANTS
C     ARGUMENTS 
      integer nbins
      real*8 z(zdim)
      real*8 hErec_raw(maxnbin,nmode,-3:3,-3:3,ndetect,-1:1)
      real*8 hErec_rec(maxnbin,nmode_rec,-3:3,-3:3,ndetect,-1:1)
C     LOCAL VARIABLES 
      integer i,j,k,l1,l2
      real*8 effe,effmu,Pe2m,Pm2e
C     EXTERNAL FUNCTIONS
C     ----------
C     BEGIN CODE
C     ----------
      do l2 = -1,1,2
         do l1 = 1,ndetect
            if (l1.eq.1) then  ! SK
               effe = z(19)
               effmu = z(20)
               Pe2m = z(21)
               Pm2e = z(22)
            elseif (l1.eq.2) then  ! Oki
               effe = z(19)
               effmu = z(20)
               Pe2m = z(21)
               Pm2e = z(22)
            elseif (l1.eq.3) then  ! Korea
               effe = z(19)
               effmu = z(20)
               Pe2m = z(21)
               Pm2e = z(22)
            endif
            if (effe.gt.1d0) effe = 1d0
            if (effe.lt.0d0) effe = 0d0
            if (effmu.gt.1d0) effmu = 1d0
            if (effmu.lt.0d0) effmu = 0d0
            if (Pe2m.gt.1d0) Pe2m = 1d0
            if (Pe2m.lt.0d0) Pe2m = 0d0               
            if (Pm2e.gt.1d0) Pm2e = 1d0
            if (Pm2e.lt.0d0) Pm2e = 0d0               

            do k = -3,3
               do j = -3,3
                  do i = 1,nbins
C     e > e events
                     hErec_rec(i,1,j,k,l1,l2) = 
     &                    effe*hErec_raw(i,1,j,k,l1,l2)
c     &                    effe*(1d0 -Pe2m)*hErec_raw(i,1,j,k,l1,l2)
C     mu > mu  events
                     hErec_rec(i,2,j,k,l1,l2) = 
     &                    effmu*(1d0 -Pm2e)*hErec_raw(i,2,j,k,l1,l2)
C     Just for bokkkeeping of NC-pi0 and NC-gamma events
                     hErec_rec(i,3,j,k,l1,l2) = hErec_raw(i,3,j,k,l1,l2)
                     hErec_rec(i,4,j,k,l1,l2) = hErec_raw(i,4,j,k,l1,l2)
C     mu > e miss-ID events
                     hErec_rec(i,5,j,k,l1,l2) = 
     &                    Pm2e*hErec_raw(i,2,j,k,l1,l2)
c     &                    effmu*Pm2e*hErec_raw(i,2,j,k,l1,l2)
C     e > mu miss-ID events
                     hErec_rec(i,6,j,k,l1,l2) =
     &                    0d0
c     &                    effe*Pe2m*hErec_raw(i,1,j,k,l1,l2)
                  enddo
               enddo
            enddo
         enddo
      enddo

      return
      end
