      integer ndiv,mode,ifixL,ifluc,snmax,ismear,ihisto,ipi0unc
      common /par_sysi/ ndiv,mode,ifixL,ifluc,snmax,ismear,ihisto,ipi0unc

      integer iminuit,iinput,ithatm,iCCQE,iCCRes,iNC,ipi0dist,ipi0xsec
      common /par_sysi2/ iminuit,iinput,ithatm,iCCQE,iCCRes,iNC,ipi0dist,ipi0xsec

      integer ihfunc,iNnm,iNne,iNam,iNae,iSK,iOki,iKr
      common /par_sysi3/ ihfunc,iNnm,iNne,iNam,iNae,iSK,iOki,iKr

      real*8 Srho,SV,SL,SOAB,Orho,OV,OL,OOAB,Krho,KV,KL,KOAB
      common /par_sys4/ Srho,SV,SL,SOAB,Orho,OV,OL,OOAB,Krho,KV,KL,KOAB

      integer Sbfactor,Obfactor,Kbfactor
      real*8 Semin,Semax,Smmin,Smmax
      real*8 Oemin,Oemax,Ommin,Ommax
      real*8 Kemin,Kemax,Kmmin,Kmmax
      common /par_sys5/ Semin,Semax,Smmin,Smmax,Oemin,Oemax,Ommin,Ommax
     &	     ,Kemin,Kemax,Kmmin,Kmmax,Sbfactor,Obfactor,Kbfactor

      integer ieff,imissid
      common /par_sys4/ ieff,imissid

      integer binsize_factor
      real*8 L,serror,basic_binsize
      common /par_sysr/ L,serror,basic_binsize,binsize_factor

      integer nu_mode,beam,detect
      real*8 OAB,V,Y,r_nu,r_anu,Emin,Emax
      common /par_exp/ OAB,V,Y,r_nu,r_anu,Emin,Emax
      common /par_exp2/ nu_mode,beam,detect

      integer MH
      real*8 s2sol_2,thatm,s2rct_2,dm21_2,dm31_2,dCP,rho
      common /par_phy/ s2sol_2,thatm,s2rct_2,dm21_2,dm31_2,dCP,rho,MH

      real*8 fs2sol_2(2),fthatm(2),fs2rct_2(2),fdm21_2(2),fdm31_2(2),fdCP(2)
      common /par_fit/ fs2sol_2,fthatm,fs2rct_2,fdm21_2,fdm31_2,fdCP

      real*8 fCCQEn(2),fCCQEa(2),fCCResn(2),fCCResa(2),fpi0(2),fpirs(2),fpico(2)
      real*8 frpi0(2)
      common /err_cor/ fCCQEn,fCCQEa,fCCResn,fCCResa,fpi0,fpirs,fpico,frpi0

      real*8 fS(2),fSrho(2),fSnmn(2),fSnen(2),fSnma(2),fSnea(2)
      common /err_sk/ fS,fSrho,fSnmn,fSnen,fSnma,fSnea

      real*8 fSamn(2),fSaen(2),fSama(2),fSaea(2)
      common /err_sk2/ fSamn,fSaen,fSama,fSaea

      real*8 Seffe(2),Seffmu(2),SPe2m(2),SPm2e(2)  
      common /err_sk3/ Seffe,Seffmu,SPe2m,SPm2e  

      real*8 fO(2),fOrho(2),fOnmn(2),fOnen(2),fOnma(2),fOnea(2)
      common /err_ok/ fO,fOrho,fOnmn,fOnen,fOnma,fOnea

      real*8 fOamn(2),fOaen(2),fOama(2),fOaea(2)
      common /err_ok2/ fOamn,fOaen,fOama,fOaea

      real*8 Oeffe(2),Oeffmu(2),OPe2m(2),OPm2e(2)  
      common /err_ok3/ Oeffe,Oeffmu,OPe2m,OPm2e  

      real*8 fK(2),fKrho(2),fKnmn(2),fKnen(2),fKnma(2),fKnea(2)
      common /err_kre/ fK,fKrho,fKnmn,fKnen,fKnma,fKnea

      real*8 fKamn(2),fKaen(2),fKama(2),fKaea(2)
      common /err_kre/ fKamn,fKaen,fKama,fKaea

      real*8 Keffe(2),Keffmu(2),KPe2m(2),KPm2e(2)  
      common /err_kr3/ Keffe,Keffmu,KPe2m,KPm2e  

      integer icc,MHH
      common /other/ icc,MHH

      integer maxnbin,nout,zdim,eventdim,maxnparam
      integer ndetect,nmode,nmode_rec,nmode_like,imaxint
      parameter (maxnbin=1000,zdim=200,eventdim=4,nout=6)
      parameter (maxnparam=100,ndetect=3)
      parameter (nmode=4) ! 1:e 2:mu 3:pi0 4:gamma
      parameter (nmode_rec=6) ! 1:e>e 2:mu>mu 3:pi0 4:gamma 5:mu>e 6:e>mu
      parameter (nmode_like=2) ! 1:e-like 2:mu-like
      parameter (imaxint=4)
