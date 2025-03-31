!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!              *** KOPLING TILL EA-RUTINER ***
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      SUBROUTINE EATTVX(SI,N,G,Gtot,T,Dk, GI,SG,SN)
!***********************************************************************
!  Grundytetillv„xt och sj„lvgallring f”r tall enligt:
!  Agestam, E. 1985. En produktionsmodell f”r blandbest†nd av tall, gran
!     och bj”rk i Sverige. Rapport nr 15. Inst f”r skogsproduktion,
!     SLU. Garpenberg.
!
!  IN:   SI    =  bonitet H100 tall (m) - site index H100 (m)
!        N     =  stamantal tall (/ha) - no. of stems of pine (/ha)
!        G     =  grundyta tall (m2/ha) - basal area of pine (m2/ha)
!        Gtot  =  total grundyta (m2/ha) - total basal area (m2/ha)
!        T     =  br”sth”jds†lder vid periodens mitt (†r) - age at breast height (y)
!        Dk    =  diameter tall genom diameter tot - diameter of pine divided by total diameter (dec.)
!  UT:   GI    =  grundytetillv„xt per †r (m2/ha) - basal area growth (m2/ha and year)
!        SG    =  sj„lvgallring i grundyta per †r (m2/ha) - natural mortality in basal area (m2/ha and year)
!        SN    =  sj„lvgallrat stamantal per †r (/ha) - natural mortality in no. of stems (/ha and year)
!***********************************************************************
      REAL*4 N,A(10),BI(10),BS(4)
      DATA BI/-0.1328E-3, 0.42115, 0.37398, 0.01077, 0.01335, 0.01824, 0.4362E-2, -0.19424, -0.213E-3, 0.10502/
      DATA BS/ 0.4809, 2.2526, -3.5439, -2.6191/

!-- GRUNDYTETILLVŽXT
      A(1)=G*100.
      A(2)=LOG(A(1))
      A(3)=LOG(N)
      A(4)=0.0
      A(5)=0.0
      A(6)=0.0
      IF(T.GT.80.)THEN
         A(6)=A(3)
      ELSEIF(T.GT.60.)THEN
         A(5)=A(3)
      ELSEIF(T.GT.40.)THEN
         A(4)=A(3)
      ENDIF
      A(7)=SI*10.
      A(8)=LOG(T)
      A(9)=(Gtot-G)*100.
      A(10)=LOG(Dk)

      FUNK=-2.0065
      DO 10 I=1,10
10    FUNK=FUNK+A(I)*BI(I)
      GI=EXP(FUNK)/100.

!-- SJŽLVGALLRAD GRUNDYTA
      A(1)=A(3)
      A(2)=LOG(Gtot)
      A(3)=LOG(A(7))
      A(4)=A(8)

      FUNK=16.039
      DO 20 I=1,4
20    FUNK=FUNK+A(I)*BS(I)
      SG=EXP(FUNK)
	SG=min(SG,0.02*G)

!-- SJŽLVGALLRAT STAMANTAL
      IF(T.LT.45.)THEN
         SN=SG/G*N/0.36
      ELSE
         SN=SG/G*N/0.29
      ENDIF
 	SN=min(SN,0.02*N)

      RETURN
      END

      SUBROUTINE EAGTVX(SI,N,G,Gtot,T,Gut, GI,SG,SN)
!***********************************************************************
!  Grundytetillv„xt och sj„lvgallring f”r gran enligt:
!  Agestam, E. 1985. En produktionsmodell f”r blandbest†nd av tall, gran
!     och bj”rk i Sverige. Rapport nr 15. Inst f”r skogsproduktion,
!     SLU. Garpenberg.
!
!  IN:   SI    =  bonitet H100 gran (m) - site index H100 (m)
!        N     =  stamantal gran (/ha) - no. of stems of spruce (//ha)
!        G     =  grundyta gran (m2/ha) - basal area of spruce (m2/ha)
!        Gtot  =  total grundyta (m2/ha) - total basal area (m2/ha)
!        T     =  br”sth”jds†lder vid periodens mitt (†r) - age at breast height (y)
!        Gut   =  utgallrad grundyta vid periodens b”rjan (m2/ha) - thinned volume at the beginning of the period (m2/ha)
!  UT:   GI    =  grundytetillv„xt per †r (m2/ha) - basal area growth (m2/ha and year)
!        SG    =  sj„lvgallring i grundyta per †r (m2/ha) - natural mortality in basal area (m2/ha and year)
!        SN    =  sj„lvgallrat stamantal per †r (/ha) - natural mortality in no. of stems (/ha and year)
!***********************************************************************
      REAL*4 N,A(7),BI(7),BS(2)
      DATA BI/ -0.2309E-3, 0.67033, 0.23347, -0.34588, 0.2659E-2, -0.3366E-3, -0.02174/
      DATA BS/ 1.4344, 0.9816/

!-- GRUNDYTETILLVŽXT
      A(1)=G*100.
      A(2)=LOG(A(1))
      A(3)=LOG(N)
      A(4)=LOG(T)
      A(5)=SI*10.
      A(6)=(Gtot-G)*100.
      IF(Gut.LE.0.01)THEN
         A(7)=0.0
      ELSE
         A(7)=LOG(Gut*100.)
      ENDIF

      FUNK=-1.3933
      DO 10 I=1,7
10    FUNK=FUNK+A(I)*BI(I)
      GI=EXP(FUNK)/100.

!-- SJŽLVGALLRAD GRUNDYTA
      A(1)=A(3)
      A(2)=LOG(Gtot)

      FUNK=-16.996
      DO 20 I=1,2
20    FUNK=FUNK+A(I)*BS(I)
      SG=EXP(FUNK)
	SG=min(SG,0.02*G)

!-- SJŽLVGALLRAT STAMANTAL
      SN=SG/G*N/0.37
	SN=min(SN,0.02*N)

      RETURN
      END
      
      SUBROUTINE EABTVX(SI,N,G,Gtot,T,Ntot, GI,SG,SN)
!***********************************************************************
!  Grundytetillv„xt och sj„lvgallring f”r bj”rk enligt:
!  Agestam, E. 1985. En produktionsmodell f”r blandbest†nd av tall, gran
!     och bj”rk i Sverige. Rapport nr 15. Inst f”r skogsproduktion,
!     SLU. Garpenberg.
!
!  IN:   SI    =  bonitet H50 bj”rk (m) - site index birch H50 (m)
!        N     =  stamantal bj”rk (/ha) - no. of stems of birch (/ha)
!        G     =  grundyta bj”rk (m2/ha) - basal area of birch (m2/ha)
!        Gtot  =  total grundyta (m2/ha) - total basal area (m2/ha)
!        T     =  br”sth”jds†lder vid periodens mitt (†r) - age at breast height (y)
!        Ntot  =  totalt stamantal (/ha) - total no. of stems (/ha)
!  UT:   GI    =  grundytetillv„xt per †r (m2/ha) - basal area growth (m2/ha and year)
!        SG    =  sj„lvgallring i grundyta per †r (m2/ha) - natural mortality in basal area (m2/ha and year)
!        SN    =  sj„lvgallrat stamantal per †r (/ha) - natural mortality in no. of stems (/ha and year)
!***********************************************************************
     REAL*4 N,Ntot,A(5),BI(5),BS(2)
      DATA BI/ 0.63464, 0.17829, -0.73932, 0.2557E-2, -0.2670E-3/
      DATA BS/ 0.4199, 0.3236/

!-- GRUNDYTETILLVŽXT
      A(1)=LOG(G*100.)
      A(2)=LOG(N)
      A(3)=LOG(T)
      A(4)=SI*10.
      A(5)=(GTOT-G)*100.

      FUNK=0.4298
      DO 10 I=1,5
10    FUNK=FUNK+A(I)*BI(I)
      GI=EXP(FUNK)/100.

!-- SJŽLVGALLRAD GRUNDYTA
      A(1)=LOG(Ntot)
      A(2)=LOG(G)

      FUNK=-8.3764
      DO 20 I=1,2
20    FUNK=FUNK+A(I)*BS(I)
      SG=EXP(FUNK)
	SG=min(SG,0.02*G)

!-- SJŽLVGALLRAT STAMANTAL
      SN=SG/G*N/0.48
	SN=min(SN,0.02*N)
      RETURN
      END

	SUBROUTINE xG3VOL(IFIX,FIX,BEST,ART)
!***********************************************************************
!  Här bara statisk volymsberäkning - uppdatering av ART(Vs,.)
!  Agestam, E. 1985. En produktionsmodell f”r blandbest†nd av tall, gran
!     och bj”rk i Sverige. Rapport nr 15. Inst f”r skogsproduktion,
!     SLU. Garpenberg.
!  F”r ek och bok enligt: Hagberg, E. och Mat‚rn, B. 1975. Volymfunktioner
!  f”r st†ende tr„d av ek och bok. Materialet och dess bearbetning.
!  Rapporter och uppsatser nr 15, Institutionen f”r skoglig matematisk
!  statistik, Skogsh”gskolan. Stockholm!***********************************************************************
 	USE G3_Global
    USE G3_NAMES
	USE G3_interfaces
      INTEGER :: IFIX(20)
      REAL    :: FIX(20)
      REAL    :: BEST(20),ART(7,MXSPECI)
!      
		INTEGER	:: i
    	REAL	:: EATVOL,EAGVOL,EABVOL

	do i=1,MXSPECI
	    IF( ART(Gs,i) > 0.1 )THEN
			IF( i.EQ.Pine .or. i .EQ. Contorta )THEN		! TALL
      			ART(Vs,i) =  EATVOL(FIX(Lat),FIX(SI),ART(Ns,i),ART(Gs,i),ART(BHAs,i),BEST(N))
   	 		ELSEIF( i .EQ. Spruce )THEN		! GRAN
      			ART(Vs,i) =  EAGVOL(FIX(Lat),FIX(SI),ART(Ns,i),ART(Gs,i),ART(BHAs,i),BEST(N))
            ELSEIF( i .EQ. Beech .or.  i .EQ. Oak )THEN
            	Dt = G3DIA(art(Gs,i),art(Ns,i))
                Ht = ART(Hs,i)
         		if(i == Beech) &
           	ART(Vs,i) = ( 0.01275*Dt*Dt*Ht+0.12368*Dt*Dt+0.0004701*Dt*Dt*Ht*Ht+0.00622*Dt*Ht*Ht ) * ART(Ns,i) / 1000. 
         		if(i == Oak) &
         	ART(Vs,i) = ( 0.03522*Dt*Dt*Ht+0.08772*Dt*Ht-0.04905*Dt*Dt ) * ART(Ns,i) / 1000.
    		ELSE! LÖV = BJÖRK
      			ART(Vs,i) =  EABVOL(FIX(Lat),FIX(SI),ART(Ns,i),ART(Gs,i),ART(BHAs,i),BEST(N))
    		endif
    	ENDIF
 	enddo

	return
    end
 
      Real FUNCTION EATVOL(LAT,SI,N,G,T,Ntot)
!***********************************************************************
!  Statisk volymsber„kning f”r tall - funktion T2 - enligt:
!  Agestam, E. 1985. En produktionsmodell f”r blandbest†nd av tall, gran
!     och bj”rk i Sverige. Rapport nr 15. Inst f”r skogsproduktion,
!     SLU. Garpenberg.
!
!  IN:   LAT   =  latitud (o) - latitude (degrees)
!        SI    =  bonitet H100 tall (m) - site index pine H100 (m)
!        N     =  stamantal tall (/ha) - no. of stems pine (/ha)
!        G     =  grundyta tall (m2/ha) - basal area pine (m2/ha)
!        T     =  br”sth”jds†lder (†r) - age at breast height (y)
!        Ntot  =  totalt stamantal (/ha) - total no. of stems (/ha)
!  UT:   EATVOL=  volym (m3sk/ha) - volume (m3sk/ha)
!***********************************************************************
      REAL*4 LAT,N,Ntot,A(6),B(6)
      DATA B/ 1.0772, 0.7859, 0.3131, -7.4681, -0.06444, 0.4741/

		EATVOL=0.
		IF(G == 0.)RETURN
        
      A(1)=LOG(G)
      A(2)=LOG(SI*10.)
      A(3)=LOG(T)
      A(4)=1./T
      A(5)=LOG(N)
      A(6)=LOG(LAT)

      FUNK=-5.0965
      DO 10 I=1,6
10    FUNK=FUNK+A(I)*B(I)
      EATVOL=EXP(FUNK)

      RETURN
      END

      Real FUNCTION EAGVOL(LAT,SI,N,G,T,Ntot)
!***********************************************************************
!  Statisk volymsber„kning f”r gran - funktion G2 - enligt:
!  Agestam, E. 1985. En produktionsmodell f”r blandbest†nd av tall, gran
!     och bj”rk i Sverige. Rapport nr 15. Inst f”r skogsproduktion,
!     SLU. Garpenberg.
!
!  IN:   LAT   =  latitud (o) - latitude (degrees)
!        SI    =  bonitet H100 gran (m) - site index spruce H100 (m)
!        N     =  stamantal gran (/ha) - no. of stems spruce (/ha)
!        G     =  grundyta gran (m2/ha) - basal area spruce (m2/ha)
!        T     =  br”sth”jds†lder (†r) - age at breast height (y)
!        Ntot  =  totalt stamantal (/ha) - total no. of stems (/ha)
!  UT:   EAGVOL=  volym (m3sk/ha) - volume (m3sk/ha)
!***********************************************************************
      REAL*4 LAT,N,Ntot,A(6),B(6)
      DATA B/ 1.2010, 0.6476, 0.1581, -0.01084, -0.002148, -11.1378/

		EAGVOL=0.
		IF(G == 0.)RETURN

      A(1)=LOG(G)
      A(2)=LOG(SI*10.)
      A(3)=LOG(T)
      A(4)=SQRT(N)
      IF(Ntot-N.GT.1.)THEN
         A(5)=SQRT(Ntot-N)
      ELSE
         A(5)=0.0
      ENDIF
      A(6)=1./T

      FUNK=-2.1739
      DO 10 I=1,6
10    FUNK=FUNK+A(I)*B(I)
      EAGVOL=EXP(FUNK)

      RETURN
      END


      Real FUNCTION EABVOL(LAT,SI,N,G,T,Ntot)
!***********************************************************************
!  Statisk volymsber„kning f”r gran - funktion B2 - enligt:
!  Agestam, E. 1985. En produktionsmodell f”r blandbest†nd av tall, gran
!     och bj”rk i Sverige. Rapport nr 15. Inst f”r skogsproduktion,
!     SLU. Garpenberg.
!
!  IN:   LAT   =  latitud (o) - latitude (degrees)
!        SI    =  bonitet H50 bj”rk (m) - site index birch H50 (m)
!        N     =  stamantal björk (/ha) - no. of stems birch (/ha)
!        G     =  grundyta björk (m2/ha) - basal area birch (m2/ha)
!        T     =  br”sth”jds†lder (†r) - age at breast height (y)
!        Ntot  =  totalt stamantal (/ha) - total no. of stems (/ha)
!  UT:   EABVOL=  volym (m3sk/ha) - volume (m3sk/ha)
!***********************************************************************
      REAL*4 LAT,N,Ntot,A(5),B(5)
      DATA B/ 1.0789, 0.9233, 0.4574, -0.04264, 0.6905/

		EABVOL=0.
		IF(G == 0.)RETURN

      A(1)=LOG(G)
      A(2)=LOG(SI*10.)
      A(3)=LOG(T)
      A(4)=LOG(N)
      A(5)=LOG(LAT)

      FUNK=-7.3526
      DO 10 I=1,5
10    FUNK=FUNK+A(I)*B(I)
      EABVOL=EXP(FUNK)

      RETURN
      END

