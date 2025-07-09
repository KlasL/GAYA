      SUBROUTINE G3URES(IP1,IP2,CostSilv,CostHarv,restim,resmav,resreject,IERR)
!***********************************************************************
!  RUTIN F™R BERŽKNING AV UTBYTE OCH RESURTGNG. ANROPAS EFTER DET
!  PROGNOS GJORTS UNDER DET AKTUELLA BEHANDLINGSPROGRAMMET.
!  PROGNOSPERIODEN OMFATTAR PERIOD 1 TOM IP2. F™R PERIODERNA 1 TOM IP1-1
!  ŽR VŽRDENA I FŽLTEN BEST, IART, ART CUT OCH NRATG IDENTISKA MED DE
!  F™R DET F™REGENDE BEHANDLINGSPROGRAMMET OCH BERŽKNINGAR F™R DESSA
!  PERIODER BEH™VER SLEDES INTE UPPREPAS. RESULTATET LŽGGS LŽMPLIGEN
!  P ETT EGET COMMONBLOCK SOM KAN UTNYTTJAS AV G3OUT. UTSKRIFT AV
!  RESULTATET KAN G™RAS HŽR I STŽLLET F™R I G3OUT. G3OUT G™RS D TILL
!  DUMMY-RUTIN.
!
!  PARAMETRAR:
!     IP1   = (in) f”rsta period f”r vilken nya v„rden tilldelats BEST,
!             IART, ART, NRATG och CUT
!     IP2   = (in) sista period f”r vilken ber„kningar skett
!     IERR  = (ut) s„tts till 1 om G3OUT inte skall anropas efter G3URES
!             annars 0
!
!  RES-f„lt:
!     1     = Total cost
!***********************************************************************
!-- GAYA-DEKLARATIONER
	USE G3_Global
	USE G3_GAPER
	USE G3_GAATG
	USE G3_GAFRAM
    USE G3_NAMES
	USE G3_interfaces
	INTEGER ::	IP1,IP2,IERR
    REAL    :: CostSilv(MXPER),CostHarv(MXPER), &
        restim(MXSPECI,MXPER),resmav(MXSPECI,MXPER),resreject(MXPER)

!-- DEKLARATIONER UTBYTESBERŽKNING
      REAL*4 SORT(4),Timber,Pulpwood,Reject
!-- EGNA ALLMŽNNA DEKLARATIONER
 	  REAL :: HarvTimeFi,ForwTimeFi,HarvHcostTh,ForwHcostTh,HarvHcostFF,ForwHcostFF,RootRot
 	  REAL :: CleaningHcost,FertilizationCost,ThinningPremium
	  DATA	HarvHcostTh,ForwHcostTh,HarvHcostFF,ForwHcostFF/1100.,900.,1350.,1070./
	  DATA	CleaningHcost/375./,FertilizationCost/20./,ThinningPremium/0./
! Skogsv†rdskostnad bon 12- 8BioFrameCom.txt
      INTEGER :: SIclass
      REAL*4   skvc(5)
	  DATA skvc/4050.,5400.,6750.,8100.,9450./					! Skogforsk-resultat nr7_2001 
! Qualities and transport (from 
        REAL ::	MinTimberLoad(MXSPECI), TransferToPulpwood(MXSPECI)
        DATA    MinTimberLoad/MXSPECI*20./
        DATA	TransferToPulpwood/0.,0.,0.6,8*1.,0.1/  ! Vanlig björk utgångsantagande 0.6, känslighetsanalys 0.3;
! Biomass
		REAL ::	StemToTScoar
        DATA    StemToTScoar/0.027/ ! Stems left on harvesting site
! Start
      DATA     ISTART/1/

      

!-- F™RSTA VŽNDAN
      IF(ISTART.EQ.1)THEN
         ISTART=0
      ENDIF

!-- BERŽKNINGAR F™R PERIOD IP1 TOM IP2 ---------------------------------

      IERR=0

      DO 100 IPER=IPER1,IP2,iper2

         CostSilv(IPER)=0.0
         CostHarv(IPER)=0.0
         restim(:,IPER) = 0.0
         resmav(:,IPER) = 0.0
         resreject(IPER) = 0.0
         VTOT = 0.

!-- TGŽRDSOBEROENDE UPPGIFTER
         IATYP=IXATG(NRATG(IPER))
!-- SKOGSVÅRD (ing. tillstånd eller vid slutavverkning
		if(IATYP == IP)then
        	CostSilv(IPER) = CostSilv(IPER) +	9434. + 484.*(nint(FIX(SI))-22)
        	CostSilv(IPER) = CostSilv(IPER) + &	
            	(ARTout(Ns,HybAsp,IPER)+ARTout(Ns,Poppel,IPER))*(1.5+4.)	! Lägg till planterade*(plantering+planta)
         	CostSilv(IPER) = CostSilv(IPER)*exp(0.03*BESTout(TotAge,IPER))
        endif
!-- RÖJNING
 		IF(IATYP.EQ.Cl)THEN
        	CostSilv(IPER) = CostSilv(IPER) +	&
            (1.625 + 0.000775*SUM(CUT(2,:,IPER)))*CleaningHcost	! Efter Tabell 3, Skogforsk, Redogörelse nr 1, 2007
 !-- GALLRING o SLUTAVVERKNING
        ELSEIF(IATYP.EQ.Th.OR.IATYP.EQ.FT.OR.IATYP.EQ.FF)THEN
            DO 30 I=1,MXSPECI
			  IF(CUT(1,I,IPER).GT.1.)THEN
               		CALL UTBYTE(I,CUT(1,I,IPER),CUT(3,I,IPER),CUT(4,I,IPER), SORT)
                	Timber = SORT(1)+SORT(2)
                	Pulpwood = SORT(3)
! Transfer of timber to reject and pulpwood
				Reject = 0.
				if( i >= Birch)Reject = Timber*RootRot(i,CUT(UHs,i,IPER),ARTin(BHAs,i,IPER),CUT(UDs,i,IPER))
                !resreject(IPER) = resreject(IPER) + Reject
                Timber = Timber - Reject
                Pulpwood = Pulpwood + Timber*TransferToPulpwood(I) + Reject ! Last term if Reject goes to pulpwood 
               	Timber = Timber*(1.-TransferToPulpwood(I))
               if( Timber < MinTimberLoad(I))then
                  Pulpwood = Pulpwood + Timber
                  Timber = 0.
                endif
     			restim(I,IPER) = restim(I,IPER) + Timber
     			resmav(I,IPER) = resmav(I,IPER) + Pulpwood
                if(IATYP == FF)then
					CostHarv(IPER) = CostHarv(IPER)	&
     					+ HarvTimeFi(1,I,2,CUT(1,I,IPER)/CUT(2,I,IPER))	&
     					* CUT(1,I,IPER)* HarvHcostFF
                else
					CostHarv(IPER) = CostHarv(IPER)	&
     					+ HarvTimeFi(2,I,2,CUT(1,I,IPER)/CUT(2,I,IPER))	&
     					* CUT(1,I,IPER)* HarvHcostTh
				endif
              ENDIF
30          CONTINUE
            VTOT=sum(restim(:,IPER)) + sum(resmav(:,IPER))
            if(IATYP == FF)then
				CostHarv(IPER) = CostHarv(IPER)+ForwTimeFi(1,1,1,VTOT)*ForwHcostFF*VTOT
            else           
				CostHarv(IPER) = CostHarv(IPER)+ForwTimeFi(2,1,1,VTOT)*ForwHcostTh*VTOT
            endif

        ENDIF

        do i=1,MXSPECI
            restim(I,IPER) = restim(I,IPER)*(1.-StemToTScoar)
            resmav(I,IPER) = resmav(I,IPER)*(1.-StemToTScoar)
        enddo

!-- Gödsling
      	IF( IATYP.EQ.Fe .or. IATYP .eq. FT )then
			CostSilv(IPER) = CostSilv(IPER) + ATG(FeKg,0,NRATG(IPER))*FertilizationCost
		endif
 
100   CONTINUE

      RETURN

!-- ALTERNATIV EJ F™R UTSKRIFT I G3OUT

99    IERR=1

      RETURN
      END

      SUBROUTINE UTBYTE(I,V,D,H, OUT)
!--------------------------------------------------------------------
!  Ger utbyte och v„rde. V„rden i f”ljande f„lt anv„nds (ber„knade
!  med Ollas' best†ndsmetod i separat program; f”r tall resp gran):
!        GF    = m3fub gagnvirke/m3sk
!        TAND  = m3fub timmer/m3sk
!        IDIM  = index f”r ing†ng i prislista (toppdiaklass)
!        TF    = toppformtal timmer
!
!        (in) I    = trädslag => (1= tall; 2= gran; 3= annat)
!        (in) V    = volym (m3sk)
!        (in) D    = diameter (cm)
!        (in) H    = h”jd (m)
!        (in) OSA  = o/s-andel som andel av timmer
!        (ut) OUT(1) = m3fub o/s per ha
!        (ut) OUT(2) = m3fub kvinta per ha
!        (ut) OUT(3) = m3fub massav per ha
!        (ut) OUT(4) = v„rde kr per ha
!--------------------------------------------------------------------
      REAL*4 OUT(4)
      INTEGER :: ITS
      REAL :: mmin(3),tmin(3)
      DATA mmin/5.,5.,5./, tmin/12.,12.,18./

5     OUT(:) =0.0
	  ITS = min(I,3)

      ireg=1
      CALL VDUB(IREG,ITS,V,D,H,VUB,DUB)
      CALL OLLAB("B",DUB,H,MMIN(ITS),TMIN(ITS),G3,GF,TAND,TDIM,TF)

      OUT(1)=TAND*VUB
      OUT(3)=GF*VUB-OUT(1)

      RETURN
      END

      SUBROUTINE VDUB(IREG,ITS,V,D,H,VUB,DUB)
!--------------------------------------------------------------------
!        Ber„kning av diameter och volym under bark med N„slund.
!        Volymsber„kningen ub samma som anv„nds i HUGIN-systemet.
!
!        (in) IREG = Norra Sv=1; S”dra Sv=2
!        (in) ITS  = tr„dslag (1= tall; 2= gran; 3= bj”rk)
!        (in) V    = volym (m3sk)
!        (in) D    = diameter (cm)
!        (in) H    = h”jd (m)
!        (ut) VUB  = volym (m3sk ub)
!        (ut) DUB  = diameter (cm ub)
!--------------------------------------------------------------------
      REAL*4 BARK(2,2,2),VP(4,2,2),VU(4,2,2)

      DATA BARK/0.2,0.10,	&
                0.5,0.05,	&
                0.0,0.15,	&
                0.5,0.05/
      DATA VP/0.09314, 0.03069, 0.002818,  0.00000,	&
              0.12020, 0.01504, 0.023410, -0.06590,	&
              0.10720, 0.02427, 0.007315,  0.00000,	&
              0.11040, 0.01925, 0.018150, -0.04936/
      DATA VU/0.05491, 0.03641, 0.002699,  0.00000,	&
              0.11530, 0.01522, 0.021700, -0.05501,	&
              0.06271, 0.03208, 0.005725,  0.00000,	&
              0.10760, 0.01929, 0.017230, -0.04615/

!-- DEFINIERA INDEX
      NS=IREG
      JTS=ITS
      IF(JTS.EQ.3)JTS=1

!-- DIAMETER UB
      DUB=D*(1.-BARK(2,JTS,NS))-BARK(1,JTS,NS)

!-- VOLYM
      D21=D*D
      D22=DUB*DUB
      H2 =H*H
      V1=VP(1,JTS,NS)*D21+VP(2,JTS,NS)*D21*H+VP(3,JTS,NS)*D*H2+VP(4,JTS,NS)*H2
      V2=VU(1,JTS,NS)*D22+VU(2,JTS,NS)*D22*H+VU(3,JTS,NS)*DUB*H2+VU(4,JTS,NS)*H2
      VUB=V*V2/(V1+0.001)

      RETURN
      END

      SUBROUTINE OLLAB(V,DGV,HGV,MMIN,TMIN,G3,GF,TAND,TDIM,TFORM)
!--------------------------------------------------------------------
!     OLLAs best†ndsvisa metod.Rutinen avser ber„kningen av utbytet
!     f”r ett tr„dslag för ett bestånd (variant = "B") eller träd ( "T" ) 
!     Variabler:DGV=grundytev{gd medeldiameter
!               HGV=    "        medelh”jd
!               MMIN=minsta massavedsdiameter
!               TMIN=minsta timmerdiameter
!               G3=gagnvirkesandel - 3 m massaved
!               GF=gagnvirkesandel - fallande massaved
!               TAND=timmerandel
!               TDIM=timrets medeldiameter
!               TFORM=timrets toppformtal
!--------------------------------------------------------------------
      CHARACTER :: V
      REAL*4 MMIN

      IF(DGV-1.LE.MMIN)THEN
         G3=0.14
         GF=0.69
      ELSE
         G3=1.-.86/(DGV-MMIN)
         GF=1.-.31/(DGV-MMIN)
      ENDIF
      if(V == "T")then
         TAND = 1.-0.842/(DGV-2.-TMIN) + 58.2/((DGV-2.)*HGV) - 8.7*TMIN/((DGV-2.)*HGV)
      else
         TAND=.86-.6*TMIN/DGV+.009*DGV-.01*TMIN
      endif
      IF(TAND.LT.0.0)TAND=0.0
      IF(TAND.GT.MIN(G3,GF))TAND=MIN(G3,GF)
      TDIM=3.+.52*DGV+.43*TMIN
      TFORM=1.29-.009*HGV+.003*DGV

      RETURN
      END

      SUBROUTINE SKORSA(ITS,V,D,T,C)
!-----------------------------------------------------------------------
!  Tid och kostnad: SK™RDARE SLUTAVV
!  Enligt: Omr†deskalkyler mellersta Sverige 86/87
!
!  (in) ITS    = tr„dslag (1=tall, 2=gran, >2=l”v)
!  (in) V      = volym fr trdslaget (m3sk/ha)
!  (in) D      = grundytev„gd medeldiameter (cm)
!  (ut) T      = tid (tim/ha)
!  (ut) C      = kostnad inlejd kapacitet (kr/ha)
!-----------------------------------------------------------------------
      INTEGER*4 DMIN,DMAX
      PARAMETER (DMIN=12, DMAX=32, IDD=(DMAX-DMIN)/2+1)
      REAL*4 DIA(IDD),TID(IDD)
      DATA DIA/12.,14.,16.,18.,20.,22.,24.,26.,28.,30.,32./
      DATA CPT/800./
      DATA TID/.100,.090,.080,.070,.060,.055,.050,.047,.045,.046,.048/

      IF(V.EQ.0.0)THEN
         T=0.0
         C=0.0
         RETURN
      ENDIF

      IF(D.LE.DIA(1))THEN
         X=(DIA(1)-D)/2.
         T=TID(1)+(TID(1)-TID(2))*X
      ELSEIF(D.GE.DIA(IDD))THEN
         T=TID(IDD)
      ELSE
         ID=D
         ID=(MIN(DMAX-1,MAX(DMIN,ID))-DMIN)/2+1
         X=(DIA(ID+1)-D)/2.
         T=TID(ID)*X+TID(ID+1)*(1.-X)
      ENDIF

      T=T*V
      C=T*CPT

      RETURN
      END

      SUBROUTINE SKORGA(ITS,V,D,T,C)
!-----------------------------------------------------------------------
!  Tid och kostnad: SK™RDARE GALLRING
!  Enligt: Omr†deskalkyler mellersta Sverige 86/87
!
!  (in) ITS    = tr„dslag (1=tall, 2=gran, >2=l”v)
!  (in) V      = volym fr trdslaget i uttaget (m3sk/ha)
!  (in) D      = grundytev„gd medeldiameter (cm)
!  (ut) T      = tid (tim/ha)
!  (ut) C      = kostnad inlejd kapacitet (kr/ha)
!-----------------------------------------------------------------------
      INTEGER*4 DMIN,DMAX
      PARAMETER (DMIN=12, DMAX=32, IDD=(DMAX-DMIN)/2+1)
      REAL*4 DIA(IDD),TID(IDD)
      DATA DIA/12.,14.,16.,18.,20.,22.,24.,26.,28.,30.,32./
      DATA CPT/700./
      DATA TID/.260,.190,.140,.110,.090,.080,.075,.070,.065,.060,.060/

      IF(V.EQ.0.0)THEN
         T=0.0
         C=0.0
         RETURN
      ENDIF

      IF(D.LE.DIA(1))THEN
         X=(DIA(1)-D)/2.
         T=TID(1)+(TID(1)-TID(2))*X
      ELSEIF(D.GE.DIA(IDD))THEN
         T=TID(IDD)
      ELSE
         ID=D
         ID=(MIN(DMAX-1,MAX(DMIN,ID))-DMIN)/2+1
         X=(DIA(ID+1)-D)/2.
         T=TID(ID)*X+TID(ID+1)*(1.-X)
      ENDIF

      T=T*V
      C=T*CPT

      RETURN
      END

      SUBROUTINE SKOTSA(V,T,C)
!-----------------------------------------------------------------------
!  Tid och kostnad: SKOTARE SLUTAVVERKNING
!  Enligt: Omr†deskalkyler mellersta Sverige 86/87
!
!  (in) V      = volym totalt i uttaget (m3sk/ha)
!  (ut) T      = tid (tim/ha)
!  (ut) C      = kostnad inlejd kapacitet (kr/ha)
!-----------------------------------------------------------------------
      DATA CPT/600./, TID/0.1/	!Gamla värden; CPT=400, Nya från F.Wester2001 (mdlskotare,cpt=569--->stor=600, liten=500), Stud.upps.nr47,skogsteknologi
      T=TID*V
      C=T*CPT

      RETURN
      END

      SUBROUTINE SKOTGA(V,T,C)
!-----------------------------------------------------------------------
!  Tid och kostnad: SKOTARE GALLRING
!  Enligt: Omr†deskalkyler mellersta Sverige 86/87
!
!  (in) V      = volym totalt i uttaget (m3sk/ha)
!  (ut) T      = tid (tim/ha)
!  (ut) C      = kostnad inlejd kapacitet (kr/ha)
!-----------------------------------------------------------------------
      DATA CPT/500./, TID/0.12/		!Gamla värden; CPT=400, Nya från F.Wester2001, Stud.upps.nr47,skogsteknologi
      T=TID*V
      C=T*CPT

      RETURN
      END
