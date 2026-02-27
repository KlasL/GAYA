    SUBROUTINE G3TVX(IPER)
!***********************************************************************
!***********************************************************************
!-- GAYA-DEFINITIONER
        USE G3_GAFRAM
        USE G3_GAATG
        USE G3_NAMES
        use G3_interfaces
 !-----------------------------------------------------------------------
    INTEGER ::  IPER
!-- LOCAL
    INTEGER ::  IFPER
    REAL    ::  hd
	DATA        hd/7./                ! dominant height for switch to establised forest
    
    IFPER = IPER+1
    if(BESTout(N,IPER) == 0.)then
       ARTin(:,:,IFPER)=0.0
!   elseif(BESTout(G,IPER) > 0.1 .and. BESTout(Hdom,IPER) > hd )then
   else
      call ETVX(IPER,ATG(1,0,NRATG(IPER)),IFIX,FIX,  &
        BESTout(1,IPER),ARTout(1,1,IPER),BESTin(1,IFPER),ARTin(1,1,IFPER))
!	else
!      call RTVX(IPER,ATG(1,0,NRATG(IPER)),IFIX,FIX,  &
!        BESTout(1,IPER),ARTout(1,1,IPER),BESTin(1,IFPER),ARTin(1,1,IFPER))
     endif
 
    RETURN
    END

      SUBROUTINE ETVX(IPER,ATG,IFIX,FIX,BEST1,ART1,BEST2,ART2)
!***********************************************************************
!  BESTŽMMER UTGENDE TILLSTND EFTER EN FEMRSPERIOD.
!  FRN UPPGIFTER OM TGŽRD (ID, ATG, CUT), FASTA UPPGIFTER (FIX) OCH
!  IGENDE TILLSTND (IART1, BEST1, ART1) G™RS TILLVŽXTBERŽKNING F™R
!  EN FEMRSPERIOD OCH TILLDELAS SAMTLIGA FŽLT I IART2, BEST2 OCH
!  ART2 NYA VŽRDEN.
!
!  PARAMETRAR:
!     IPER  = (in) aktuell period
!     ID    = (in) †tg„rdskod
!     ATG   = (in) †tg„rdsdefinition
!     CUT   = (in) uppgifter om avverkningsuttag
!     FIX   = (in) fixa data f”r tillv„xtfunktion
!     IART1 = (in) tr„dslagskoder f”r motsvarande f„lt i CUT och ART1
!     BEST1 = (in) best†ndsdata
!     ART1  = (in) tr„dslagsvisa data
!     IART2, BEST2, ART2 = (ut) df som IART1, BEST1 och ART1
!***********************************************************************
!-- GAYA-DEFINITIONER
        USE G3_Global
        USE G3_NAMES
implicit none      
        INTEGER :: IPER,IFIX(NFIX)
        REAL    :: ATG(NTREAT),FIX(NFIX)
        REAL    :: BEST1(NBEST),ART1(NART,MXSPECI)
        REAL    :: BEST2(NBEST),ART2(NART,MXSPECI)
!-----------------------------------------------------------------------

!-- Lokala
		INTEGER	:: i,DomSpec,roj
		REAL	:: GI(MXSPECI),SG(MXSPECI),SN(MXSPECI),IG(MXSPECI),IN(MXSPECI),		&
            HuCGYTVX,BPSJGGRY,gpb,rda,gpu,gpa,n2sj(6),xb,hd
        REAL	:: VTVX,X,CCGROWTHEFFECT,VOLYM,GY,HEIGHTGROWTH,MORTPER5Y,GEFF
        REAL	:: BREEDING,HojdYoung,DiaYoung,BPSJGALL,BPSJGFAS,G3VOL,G3YTA
        REAL	:: rMIXe,Atot,T13,Dia1,Dia2,GB,NB,VB,PinShare,SprShare,HeurekaHojd,A1
        REAL	:: TvxDiaFactor(MXSPECI),TvxHojdFactor(MXSPECI),YoungDiaFactor(MXSPECI),eps
		DATA        hd/7./                ! dominant height for switch to establised forest
        DATA		eps/1.e-6/
    	DATA        MortPer5y/0.023/    ! Data från tidig SKA/AVB 
        DATA	TvxHojdFactor / 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1.1, 1.1/
        DATA	TvxDiaFactor/ 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1.1, 1.1/
        DATA	YoungDiaFactor/1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1./	! To adjust the young forest growth
        
        logical	:: SpecGThd(MXSPECI)

!-- NOLLSTŽLL
	BEST2(:)=BEST1(:)
	ART2(:,:)=ART1(:,:)
GI(:)=0.; SG(:)=0.; SN(:)=0.; IN(:)=0.; IG(:)=0.; GEFF=0.; 

!-- Separate species on height
	SpecGThd(:)=.false.
 	do i=1,MXSPECI
    	if(art2(Hs,i) >= hd)SpecGThd(i)=.true.
    enddo

!-- Growth projection of species with height < hd ----------------------

   	roj=0	! No PCT
    if(BEST2(TimeCl) < BEST2(TotAge) + 0.1)roj=2	! PCT more than 5 years ago
    if(BEST2(TimeCl) < 2.5)roj=1	! PCT within 5 years
 	do i=1,MXSPECI
		if(.not.SpecGThd(i) .and. art2(Ns,i) > 0.) then
            ART2(Ms,i)=ART2(Vs,i)*MortPer5y           
            ART2(Ns,i)=ART2(Ns,i)*(1.-MortPer5y)            
			ART2(BHAs,i)=ART2(BHAs,i)+5.
            Atot=ART1(BHAs,i)+T13(i,FIX(SI))
            ART2(Hs,i) = ART1(Hs,i) + HojdYoung(i,FIX(SI),Atot)*TvxHojdFactor(i)
            Dia1=DiaYOUNG(i,roj,ART1,IFIX,FIX)*TvxDiaFactor(i)
            Dia2=DiaYOUNG(i,roj,ART2,IFIX,FIX)*TvxDiaFactor(i)
            ART2(Gs,i)= ART1(Gs,i) + &
            	( G3YTA(Dia2,ART2(Ns,i))-G3YTA(Dia1,ART2(Ns,i)) )*YoungDiaFactor(i)
            if(ART2(Gs,i) < eps )ART2(:,i)=0.
            ART2(Vs,i)= G3VOL(I,ART2,IFIX,FIX)
        endif
	enddo

!-- Growth projection of species with height >= hd ---------------------

if(any(SpecGThd))then

GB = sum(ART1(Gs,:))
NB = sum(ART1(Ns,:))
VB = sum(ART1(Vs,:))
PinShare = ART1(Gs,Pine)/GB
SprShare = ART1(Gs,Spruce)/GB

! Elfving single tree growth model without Weibull
	call G3ElfvingMeanTree(IFIX,FIX,BEST2(TimeTh),ART1, GI)	

! Multiply with breeding factor
	if( BEST1(PerFF) > 0.9 )then
    	do i=1,MXSPECI
        	GI(I)=GI(I)*(1. + BREEDING(BEST1(PerFF)*5.,i))
        enddo
    endif
! Multiply with climate effect and growth improvement
   	do i=1,MXSPECI
      GI(I)=GI(I)*(1. + CCGrowthEffect(IFIX(CCscenario),IFIX(Zone),(IPER-1)*5,i) )
      GI(I)=GI(I)*TvxDiaFactor(i)**2
    enddo

!-- Avg†ng faktisk utveckling ------------------------------------------
         x=(-1. + GB/BPSJGGRY(BEST1(TotAge),PinShare,SprShare,	&
                                FIX(SI)*10.,FIX(SI)*10.,NB) )/0.1
         x=tanh(x)
         x=(1.-x)/2.
         gpu = BPSJGFAS(BEST1(TotAge),GB,PinShare,SprShare,FIX(SI)*10.,FIX(SI)*10.)
         do i=1,MXSPECI
         	gpb = BPSJGALL(i,GB,NB,BEST1(TotAge),IFIX(Zone))
       		gpa=(x*gpb+(1.-x)*gpu)
			SG(I)=ART1(Gs,i)*gpa
			SN(I)=ART1(Ns,i)*gpa
      	enddo

!-- Ingrowth (provisional model)
	call WikbvergIngrowth(IFIX,FIX,BEST1(TotAge),ART1, IN,IG)
    do i=1,MXSPECI
    	if(i==HybAsp .or. i==Poppel)then	! Bred and planted species should not have ingrowth
        	IN(i)=0.
        	IG(i)=0.
        endif
    enddo

!-- Fertilization
	IF(ATG(FeKg) > 0.0)THEN
    	DomSpec = 0
    	if( PinShare+SprShare > 0.7 .and. PinShare > SprShare )DomSpec = 1
    	if( PinShare+SprShare > 0.7 .and. PinShare <= SprShare )DomSpec = 2
      	VTVX = sum(GI(:))*VB/GB
      	CALL PetterssonGODEFF(ATG(FeType),ATG(FeKg),	&
      		FIX(Lat),FIX(ASL),IFIX(Ftype),IFIX(Moist),BEST1(TotAge),FIX(SI),DomSpec,VTVX,	&
      		BEST1(Geff1),BEST1(Geff2) )
        BEST2(TimeFe)= 0.
	ENDIF
    GEFF = 0.
	IF(BEST2(TimeFe).LT.2.5)THEN
         GEFF=BEST2(Geff1)
	ELSEIF(BEST2(TimeFe).LT.7.5)THEN
         GEFF=BEST2(Geff2)
	ENDIF
!-- UPPDATERING AV BEST2 OCH ART2 -------------------------------

	DO 110 I=1,MXSPECI
    	IF( SpecGThd(i) )THEN
           ART2(Ms,i)= SG(I)*ART1(Vs,i)/ART1(Gs,i)						! Mortality
           IF(I.LE.Spruce)THEN											! GRUNDYTA
               ART2(Gs,I)=ART1(Gs,I)+ &
               	GI(I)*(1.+GEFF)-SG(I)+IG(I)
           ELSE
               ART2(Gs,I)=ART1(Gs,I)+ &
               	GI(I)*(1.+0.5*GEFF)-SG(I)+IG(I)
           	ENDIF
            ART2(BHAs,I)=ART1(BHAs,I)+5.								! ÅLDER
            A1 = ART1(BHAs,i)+T13(i,FIX(SI))
           	ART2(Hs,i) = ART1(Hs,i) + &									! Height
            	HeurekaHojd(i,A1,ART1(Hs,i))*TvxHojdFactor(i)
            ART2(Ns,I)=ART1(Ns,I)-SN(I)+IN(I)							! STAMANTAL
           	ART2(Vs,i)=G3VOL(I,ART2,IFIX,FIX)							! Volume
        elseif(ART1(Ns,I) > 0.)then
        	rMIXe = min(1.,ART1(Hs,i)/hd)**2
        	ART2(Gs,I) = ART1(Gs,i) + (ART2(Gs,i)- ART1(Gs,i))*(1.-rMIXe) + GI(i)*rMIXe
           	ART2(Vs,i)=G3VOL(I,ART2,IFIX,FIX)
         endif

110   CONTINUE

endif	! End section with SpecGThd being true

RETURN
END
