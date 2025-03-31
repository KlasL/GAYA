      REAL FUNCTION HuCGYTVX(SI,Hc,BHAc,Gc,Nc,TimeTh)
implicit none      
!*************************************************************
! Feb 2021 - excerpt from Hugin routines
!
!     BESKRIVNING
!     ===========
!
!     CGYTVX BERŽKNAR GRUNDYTETILLVŽXTEN FOR CONTORTA.
!
!     INPARAMETRAR
!     =============
!
!                  DUMMY-PARAMETER
!                  ™VRIGA PARAMETRAR GENOM COMMON /CINYTA/
!                  OCH COMMON /CPERIOD/
!
!     UTPARAMETRAR
!     ===========
!
!     R  CGYTVX    FUNKTIONSNAMNET  (SORT  M2/HA) <-- per year period
!
!     LOKALA VARIABLER
!     ================
!
!     R  H50      DM
!     R  HDOM     DM
!     R  T       R
!     R  G       M2/HA
!     R  P       R
!     R  RN      ANTAL/HA
!     R  TG      R
!     R  RILOG     LN(CGYTVX)
!
!     PROGRAMMERARE  THOMAS JOHANSSON  80.08.30
!
!*************************************************************
REAL	:: SI,Hc,BHAc,Gc,Nc,TimeTh
REAL	:: H50,HDOM,T,G,P,RN,TG,RILOG
!
      H50  = SI*10.0
      HDOM = Hc*10.0
      T    = BHAc
      G    = Gc
      P    = 5.0
      RN   = Nc
      TG   = Min(20.,Max(0.01,TimeTh-2.5))
!
      RILOG=-6.2264+2.3937*ALOG(H50)-1.7681*ALOG(HDOM)+	&
            7.0865E-3*T+0.3904*ALOG(T)+0.2471*ALOG(G)-	&
            0.1219*ALOG(P)+0.2849*ALOG(					&
            1-EXP(-RN*0.003))+							&
            0.4805*ALOG(								&
            1-EXP(-TG*0.6630))
      HuCGYTVX=EXP(RILOG)*P
!
      RETURN
      END
