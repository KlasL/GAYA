      SUBROUTINE PetterssonGODEFF(GM,GG,Lat,ASL,Ftype,Moist,TotAge,SI,DomSpec,VTVX, RG1,RG2)
implicit none
!***********************************************************************
!  Rutinen ger relativ gîdslingseffekt fîr fîrsta och andra 5-Ürsperiod.
!  Rutinen utgîr en redigerad variant av motsvarande rutin i HUGIN. Inga
!  Ñndringar i variabler och dylikt har gjorts, med undantag av att
!  bonitet anges av dominerande trÑdslag (tall eller gran)
!
!  Fîljande variabler anvÑnds (IN):
!  GM          =  gîdselmedel (1= urea, 2= amoniumnitrat)
!  GG          =  gîdselgiva  kg/ha)
!  Lat FIX(1)      =  breddgrad (o)
!  ASL FIX(2)      =  hîjd îver havet (m)
!  Ftype FIX(3)      =  skogstyp (se ovan)
!  Moist FIX(4)      =  fuktighet (se ovan)
!  TotAge BEST(1)     =  totalÜlder (Ür)
!  SI BEST(10)    =  bonitet tall (H100 m)
!  VTVX        =  lîpande tillvÑxt (m3sk/5Ür)
!  (UT):
!  RG1         =  gîdslingseffekt som andel av gyta fîrsta 5-Ürsperiod
!  RG2         =  gîdslingseffekt som andel av gyta andra 5-Ürsperiod
!***********************************************************************
INTEGER	:: Ftype,Moist,DomSpec
REAL	:: GM,GG,Lat,ASL,TotAge,SI,VTVX	
REAL	:: RG1,RG2
!--
INTEGER	:: IGM
REAL	:: V1,V2,FUNK5,FUNK10

!-- KONTROLL AV OM POSITIV GôDSLINGSEFFEKT
      RG1=0.0
      RG2=0.0
      if(DomSpec == 0)RETURN
      IF(VTVX.LT.1.00) RETURN
!  ôVRIGA KONTROLLER (SE ROSVALL S 101) Pè BASIS AV BONITET, STYP O FUKT
      IF(SI.GE.29.9)RETURN
      IF(Ftype.LE.2)RETURN
      IF(Moist.GE.4)THEN
         IF(Moist.GE.5)RETURN
         IF(Ftype.LE.6)RETURN
      ENDIF

!-- log fertilizer response (m3 o.b. ha-1 5 years-1) first 5 yeares
	FUNK5 =  - 5.113004
    FUNK5 = FUNK5 + 0.106000 * SI
    FUNK5 = FUNK5 + 1.719173 * ALOG10(ASL + 100.)
    if(Lat >= 61.)FUNK5 = FUNK5 + 0.119126
    if(Lat < 61. .and. Lat >= 58.)FUNK5 = FUNK5 + 0.058646
    FUNK5 = FUNK5  -0.000030 * Lat*ASL
    FUNK5 = FUNK5  -0.215528 * ALOG10(Lat*(ASL+1.))
    FUNK5 = FUNK5  -0.051843 * VTVX/5.
    if(DomSpec == 1)FUNK5 = FUNK5 + 0.781026 * ALOG10(VTVX/5.)
    if(DomSpec == 2)FUNK5 = FUNK5 + 0.0306321 * ALOG10(VTVX/5.)

    IGM=nint(GM)
    IF(IGM.EQ.1)THEN
	    FUNK5 = FUNK5 + 1.226132 * ALOG10(GG)
    	FUNK5 = FUNK5  -0.00057 * SI*GG
    ELSEIF(IGM.EQ.2)THEN
	    FUNK5 = FUNK5 + 1.335267 * ALOG10(GG)
	    FUNK5 = FUNK5  -0.000090 * SI*GG
    ENDIF

!-- total response in log response (m3 o.b. ha -1)
    FUNK10 = - 0.115848
    FUNK10 = FUNK10  +0.973479 * FUNK5
    FUNK10 = FUNK10  +0.019539 * (Lat-54.)
    FUNK10 = FUNK10  +0.004166 * (ASL+100.)
    FUNK10 = FUNK10  -0.359743 * ALOG10(ASL+100.)
    FUNK10 = FUNK10  -0.000057 * Lat*ASL
    FUNK10 = FUNK10  -0.005493 * SI
    if(DomSpec == 1)FUNK10 = FUNK10  -0.105466
    FUNK10 = FUNK10  +0.314026 * ALOG10(GG)

!-- RG1 & RG2
    V1=0.90*10.**FUNK5
    V2=0.90*10.**FUNK10 - V1
    IF(V2.LT.0) V2=0.00
	RG1=V1/VTVX
	RG2=V2/VTVX

      RETURN
      END


      SUBROUTINE RosvallGODEFF(GM,GG,Lat,ASL,Ftype,Moist,TotAge,SI,DomSpec,VTVX, RG1,RG2)
implicit none
!***********************************************************************
!  Rutinen ger relativ gîdslingseffekt fîr fîrsta och andra 5-Ürsperiod.
!  Rutinen utgîr en redigerad variant av motsvarande rutin i HUGIN. Inga
!  Ñndringar i variabler och dylikt har gjorts, med undantag av att
!  bonitet anges av dominerande trÑdslag (tall eller gran)
!
!  Fîljande variabler anvÑnds (IN):
!  GM          =  gîdselmedel (1= urea, 2= amoniumnitrat)
!  GG          =  gîdselgiva ( kg/ha)
!  Lat FIX(1)      =  breddgrad (o)
!  ASL FIX(2)      =  hîjd îver havet (m)
!  Ftype FIX(3)      =  skogstyp (se ovan)
!  Moist FIX(4)      =  fuktighet (se ovan)
!  TotAge BEST(1)     =  totalÜlder (Ür)
!  SI BEST(10)    =  bonitet tall (H100 m)
!  VTVX        =  lîpande tillvÑxt (m3sk/5Ür)
!  (UT):
!  RG1         =  gîdslingseffekt som andel av gyta fîrsta 5-Ürsperiod
!  RG2         =  gîdslingseffekt som andel av gyta andra 5-Ürsperiod
!***********************************************************************
INTEGER	:: Ftype,Moist,DomSpec
REAL	:: GM,GG,Lat,ASL,TotAge,SI,VTVX	
REAL	:: RG1,RG2
!--
INTEGER	:: I,IGM
REAL	:: V1,V2,FUNK1,FUNK2,FUNK3
	
REAL	:: AA(14),BB(9),A(14)
      DATA AA/-4.368692,-0.000694, 0.588503,-0.018376, 0.359554,	&
               0.007016, 0.424878,-0.003438, 0.412783, 0.012080,	&
               1.500548, 1.414132,-0.000105,-0.000091/,				&
           BB/ 0.592630, 0.001521,-0.566512, 0.015451, 0.281478,	&
              -0.009153,-0.011298,-0.000073, 1.019599/

!-- KONTROLL AV OM POSITIV GôDSLINGSEFFEKT
      RG1=0.0
      RG2=0.0
      if(DomSpec == 0)RETURN
      IF(VTVX.LT.1.00) RETURN
!  ôVRIGA KONTROLLER (SE ROSVALL S 101) Pè BASIS AV BONITET, STYP O FUKT
      IF(SI.GE.29.9)RETURN
      IF(Ftype.LE.2)RETURN
      IF(Moist.GE.4)THEN
         IF(Moist.GE.5)RETURN
         IF(Ftype.LE.6)RETURN
      ENDIF

!     GE INITIALV#RDEN

      FUNK1=0.0
      FUNK2=0.0
      FUNK3=0.0
 !     GG10=10.00*GG
      AA(1)=-4.368692
      AA(7)= 0.424878
!
!     G@DSLINGSEFFEKT F@RSTA 5-$RS PERIODEN
!
      A(1) =1.00
      A(2) =ASL+100.
      A(3) =ALOG10(A(2))
      A(4) =Lat-54.
      A(5) =ALOG10(A(4))
!      A(6) =SI*10.00 + 1.00
      A(6) =SI
      A(7) =ALOG10(VTVX/5.)
      A(8) =TotAge
      A(9) =ALOG10(A(8))
      A(10)=SI
      A(11)=0.00
      A(12)=0.00
      A(13)=0.00
      A(14)=0.00

	  IGM=nint(GM)
      IF(IGM.EQ.1)THEN
         A(11)=ALOG10(GG)
         A(13)=GG*A(10)
      ELSEIF(IGM.EQ.2)THEN
         A(12)=ALOG10(GG)
         A(14)=GG*A(10)
      ENDIF

      DO 10 I=1,14
10      FUNK1=FUNK1 + A(I)*AA(I)

!
!     RELATIVA GRUNDYTETILLV#XT@KNINGEN, % UNDER 5-$RSPERIODEN
!

      AA(1)=-3.067662
      AA(7)=-0.575122

      DO 20 I=1,14
20      FUNK3=FUNK3+A(I)*AA(I)


!
!     G@DSLINGSEFFEKT F@R HELA PERIODEN EXKLUSIVE F@RSTA 5-$RSPERIODEN
!

      A(5) =ALOG10(GG)
      A(7) =VTVX/5.0
      A(8) =ASL*A(4)
      A(9) =FUNK1

      DO 30 I=1,9
30      FUNK2=FUNK2 + A(I)*BB(I)

      V1=0.90*10.**FUNK1
      V2=0.90*10.**FUNK2 - V1
      IF(V2.LT.0) V2=0.00
      RG1=0.90*10.**FUNK3

!
!     RELATIVA GRUNDYTETILLV#XT@KNINGEN, % UNDER HELA PERIODEN EXKLUSI-
!     VE F@RSTA 5-$RSPERIODEN.
!

      RG2=RG1*(V2/V1)

      RETURN
      END
