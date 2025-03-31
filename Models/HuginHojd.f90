      REAL FUNCTION HuginHojdTvx(Age1,G1,V1,D1,at1,ag1,Age2,G2,V2,D2,at2,ag2)
!***********************************************************************
!  Rutinen skattar h”jdtillv„xten i ett best†nd (i brist p† b„ttre)
!  under en 5-†rsperiod. Den har brutits ut ur HUGIN-systemet d„r den
!  har nedan angiven anv„ndning.
!
!  F”ljande v„rden i BEST anv„nds:
!  1     =  total†lder (†r)
!  6     =  grundyta tr„d >= 5 cm (m2/ha)
!  7     =  volym tr„d >= 5 cm (m3sk/ha)
!  8     =  grundytemedelstammens diameter tr„d >= 5 cm (cm)
!  14    =  andel tall av total grundyta
!  15    =  andel gran av total grundyta
!**
!     RUTIN SOM BERAKNAR OVRE HOJDEN PA EN YTA
!     FUNKTIONEN HAR TAGITS FRAM AV ERIK WILHELMSSON
!     ANDERS LUNDSTROM 80-05-28
!**
!***********************************************************************
      REAL	:: Age1,G1,V1,D1,at1,ag1,Age2,G2,V2,D2,at2,ag2
!
      DIMENSION A(12),B(12)
      DATA A/ 17.722, 13.975, 30.247,   .387, 38.265,  1.048,	&
              16.578,  -.178,-31.386, -4.193,1679.941, -2.081/
!
      B(1)=V2/G2-V1/G1
      B(2)=ALOG(G2/G1)
      B(3)=ALOG(D2/D1)
      B(4)=ag2*G2-ag1*G1
      B(5)=1./V2-1./V1
      B(6)=0.0
      B(8)=5.
      B(7)=ALOG((Age2-10.)/(Age1-10.))
      B(9)=ALOG(V2/G2/V1*G1)
      B(10)=at2-at1
      B(11)=(SQRT(D2)-SQRT(D1))/100.
      B(12)=D2-D1
!
      FUNK=0.
      DO 10 I=1,12
10    FUNK=FUNK+A(I)*B(I)

      HuginHojdTvx=FUNK/10.
      IF(HuginHojdTvx.LT.0.) HuginHojdTvx=0.

      RETURN
      END

