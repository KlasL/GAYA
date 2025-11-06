    !----------------------------------------------------------------------
    MODULE NFI_data
    use G3_Global
    IMPLICIT NONE
    SAVE

    INTEGER, PARAMETER :: maxNFI=100000		! Max no. of NFI plots
    character ::    NFI_NFIid(maxNFI)*13
    real ::    NFI_Lat(maxNFI)
    real ::    NFI_ASL(maxNFI)
    integer ::    NFI_LocCli(maxNFI)
    integer ::    NFI_Ftype(maxNFI)
    real ::     NFI_Tsum(maxNFI)
    integer ::    NFI_Peat(maxNFI)
    integer ::    NFI_Ditch(maxNFI)
    integer ::    NFI_Moist(maxNFI)
    integer ::    NFI_Zone(maxNFI)
    real ::    NFI_SI(maxNFI)
    real    ::  NFI_mToRoad(maxNFI)
    integer ::    NFI_Slope(maxNFI)
    integer ::    NFI_Owner(maxNFI)
    integer ::    NFI_Hkl(maxNFI)
    real ::    NFI_Stems(MXSPECI,maxNFI)
    real ::    NFI_BA(MXSPECI,maxNFI)
    real ::    NFI_Height(MXSPECI,maxNFI) 
    real ::    NFI_Age(MXSPECI,maxNFI)
    real ::    NFI_Volume(MXSPECI,maxNFI)

    END MODULE
    !----------------------------------------------------------------------

    
    SUBROUTINE G3GET(NR,LREC,OLDFOR,M,NrDOM,GetDOM,IERR,IEND)
!***********************************************************************
!  HŽMTAR IN VŽRDEN F™R BESTND ELLER YTA SOM DEFINIERAR INGENDE
!  TILLSTND F™R PERIOD 1. INLŽSTA VŽRDEN LŽGGS I:
!  OBLIGATORISKT => FIX(.), BESTin(.,1), IART(.,1) OCH ART(.,.,1)
!  VILLKORLIGT   => IW(.), W(.)
!
!  PARAMETRAR:
!     NR    = (in) logiskt nummer (”ppnas mot fil med kommandot FSKOG)
!     LREC  = (in) recordl„ngd som den angetts med kommandot FSKOG
!     OLDFOR= (in) anger om det är etablerad skog eller ej
!     M     = (in) ordningsnummer p† best†nd/yta som h„mtas in
!     NDOM  = (in) antal definierade v„rden i fältet INDOM
!     INDOM = (in) f„lt med v„rden definierade med kommandot DOMAIN
!     IERR  = (ut) signalerar att record inte skall anv„ndas (normalt
!                  baserat p† ”verensst„mmelse mot IDOM)
!     IEND  = (ut) EOF => 1, annars 0
!***********************************************************************
!-- GAYA-DEKLARATIONER
	USE G3_Global
    use G3_GAMAIN
	USE G3_GAPER
	USE G3_GAFRAM
	USE G3_GAATG
    USE G3_NAMES
    use G3_GANEWF
    USE G3_GAPER
	USE G3_interfaces
    USE NFI_data    
    save  
!
	LOGICAL :: OLDFOR
	INTEGER :: NR,LREC,M,NrDOM,GetDOM(NrDOM),IERR,IEND,IPER,IFPER
!-- EGNA DEKLARATIONER
real    :: treatmentAgeAtStart  ! Indication of no previous treatment at start
INTEGER 	:: i,j,NoOfSpecies,DomSpecies,NewFor,Hkl
real 		:: hd,Rnd,RndLim,TBHA,s_2,roj,T13,HojdYoung,DiaYoung,DiaAtBRH
LOGICAL	:: Start,Hugin,Brandel
DATA NoOfSpecies/10/,Start/.true./,hd/10./,Hugin/.false./,Brandel/.false./                ! Övre höjd for inväxning (se RTVX)
data treatmentAgeAtStart/-99./
character :: mode*20
data mode/'NFI'/  ! Set to 'NFI' when running with NFI data and 'Stand' when with segment data
! Dummy inläsning
       real :: x
       integer :: ix
       character :: c
       logical :: l
! For volume computation of young stands
	real ::		fh(MXSPECI),NST,GY,VOLYM,VMEDEL,VSTAND,VSNED,VTOP,DomH,Vtot,Ntot
    DATA		fh/4.64,4.59,4.21,3.89,4.09,3.96,4.28,4.62,4.16,4.09,3.89,3.89/	! Form height from NFI 14-18, 7-10 m
    INTEGER :: 	its

      IERR=0
      IEND=0

!-- Read in NFI plots      
    if(Start)then
        read(68,*,end=5)
        NoNFI = 1
 		do while(Start)
            read(68,*,end=5) &
      	    NFI_NFIid(NoNFI), &	! Beståndsnummer
     	    x, &		! Class
            x, &				! Random number to select for test runs
            x, &   		! Area
            x,x, &
     	    NFI_Lat(NoNFI), &			! breddgrad (o)
     	    NFI_ASL(NoNFI), &			! hÖ”jd ”ver havet (m)
     	    NFI_LocCli(NoNFI), & 		! omr†lokalklimatiskt de(=0)
     	    NFI_Ftype(NoNFI), &		! skogstyp
!           1= h”g”rt utan ris  2= l†g”rt utan ris  3= h”g”rt med bl†b„r
!           4=l†g”rt med bl†b„r  5= h”g”rt med ris utom bl†b„r
!           6= l†g”rt med ris utom bl†b„r  7= breda gr„s  8= smala gr„s
!           9= utan f„ltskikt  10= h”g starr  11= fr„ken (sumpskogstyp)
!           12= l†g starr  13= bl†b„r  14= lingon/mj”lon  15= kr†kb„r
!           16= ljung  17= odon/skvattram  18= rosling/tranb„r
      	    NFI_Tsum(NoNFI), &		! Temperature sum
     	    NFI_Peat(NoNFI), &
     	    NFI_Ditch(NoNFI), &
     	    NFI_Moist(NoNFI), &		! Markfuktighet: 1 = Torr 2 = Frisk 3 = Frisk-fuktig 4 = Fukti
     	    NFI_Zone(NoNFI), &		! geografisk zon (1= n:a Sv, 2= m:a Sv, 3= s:a Sv)
            x,x,x, &
    	    NFI_SI(NoNFI), &
            NFI_mToRoad(NoNFI), &		! stand/plot metres from road
            NFI_Slope(NoNFI), &		! code for slope
            NFI_Owner(NoNFI), &			! code for owner
            NFI_Hkl(NoNFI), &				! Huggningsklass
            x,x, &
    	    (NFI_Stems(i,NoNFI), i=1,NoOfSpecies), & ! Stems >= 5 cm
     	    (NFI_BA(i,NoNFI), i=1,NoOfSpecies), & ! Basal area (m2)
     	    (NFI_Height(i,NoNFI), i=1,NoOfSpecies), & ! Height (m)
     	    (NFI_Age(i,NoNFI), i=1,NoOfSpecies), & ! Age at brh (total age when read, modified below)
     	    (NFI_Volume(i,NoNFI), i=1,NoOfSpecies)  ! Volume (m3sk/ha)
            NoNFI = NoNFI + 1
            if(NoNFI .gt. maxNFI)go to 5
        end do
5       NoNFI = NoNFI - 1
        Start = .false.
    endif

!-- Zero ------------------------------------------------
        IPER=1
        CFIX = ' '
        FIX = 0.0
        IFIX = 0
        ARTin(:,:,IPER) = 0.0
        BESTin(:,IPER) = 0.0

!-- Non stand values -------------------------------------------------
     	IFIX(CCscenario) = 1	! Climate change scenario: 0 = No growth effect; 1 = B2; 2 = A2
        
        ! SEKVENTIELL INLŽSNING
    IF(OLDFOR)THEN
        if(mode == 'NFI')then
           call prepareNFI(NR,NoNFI,Hkl,ierr,iend)
        elseif(mode == 'Stand')then
            call prepareStand(NR,NoNFI,Hkl,ierr,iend) 
       endif
       if (ierr == 1 .OR. iend == 1) return
         
!-- Prepare data -----------------------------
 		if(Hugin)then	! Summarize deciduous species for Hugin definitions
           	DomSpecies = Aspen
            if( ARTin(Gs,SouthBrl,1) > ARTin(Gs,DomSpecies,1) )DomSpecies = SouthBrl
            if( ARTin(Gs,OtherBrl,1) > ARTin(Gs,DomSpecies,1) )DomSpecies = OtherBrl
            if( ARTin(Gs,Larch,1) > ARTin(Gs,DomSpecies,1) )DomSpecies = Larch
            ARTin(Hs,OtherBrl,1)=ARTin(Hs,DomSpecies,1)
            ARTin(BHAs,OtherBrl,1)=ARTin(BHAs,DomSpecies,1)
        	ARTin(Ns,OtherBrl,1)=ARTin(Ns,Aspen,1) + ARTin(Ns,SouthBrl,1) + ARTin(Ns,OtherBrl,1) + ARTin(Ns,Larch,1)
        	ARTin(Gs,OtherBrl,1)=ARTin(Gs,Aspen,1) + ARTin(Gs,SouthBrl,1) + ARTin(Gs,OtherBrl,1) + ARTin(Gs,Larch,1)
        	ARTin(Vs,OtherBrl,1)=ARTin(Vs,Aspen,1) + ARTin(Vs,SouthBrl,1) + ARTin(Vs,OtherBrl,1) + ARTin(Vs,Larch,1)
            ARTin(:,Aspen,1)=0.; ARTin(:,SouthBrl,1)=0.; ARTin(:,Larch,1)=0.
        endif
! Barren land if Huggningsklass 1 (=A1)
        if( Hkl == 1 )then 	! Assume it is barren land if no or few trees
          ARTin(:,:,1) = 0.0
          BESTin(PerFF,1) = 1
        endif
! Set age if 0
   		do i=1,NoOfSpecies
        	if(	ARTin(Ns,i,1) > 0. .and. ARTin(BHAs,i,1) == 0.)ARTin(BHAs,i,1)=T13(i,FIX(SI))/2.
        enddo
! Set height if missing
   		do i=1,NoOfSpecies
        	if(	ARTin(Hs,i,1) == 0. .and. ARTin(BHAs,i,1) > 0.)ARTin(Hs,i,1) = 1.3	! Set H at BH (HojdYoung gives increment)
        enddo
! Total age to BHA        
		do i=1,NoOfSpecies
        	if(	ARTin(Ns,i,1) > 0.)then
       			ARTin(BHAs,i,1)= max(T13(i,FIX(SI))/2.,ARTin(BHAs,i,1)-T13(i,FIX(SI)))	! ålder i brösthöjd
            endif
		enddo
! Set G from V for small tress
		do i=1,NoOfSpecies
          	if(ARTin(Ns,i,1) > 0.)then
            	ARTin(Gs,i,1) = 50.
                x=1.e6
                do while ( x > ARTin(Vs,i,1) .and. ARTin(Gs,i,1) > 0.1)
                  	ARTin(Gs,i,1) = ARTin(Gs,i,1)-0.1
                	x=G3VOL(I,ARTin(:,:,1),IFIX,FIX)
                enddo
            endif
        enddo
! Volume and, if none, basal area
		do i=1,NoOfSpecies
!          	if(ARTin(Hs,i,1) > hd)then
          	if(ARTin(Hs,i,1) > 1000.)then
            	ARTin(Hs,i,1) = max(50.,ARTin(Hs,i,1))
                x=1.e6
                do while ( x > ARTin(Vs,i,1) .and. ARTin(Hs,i,1) >= hd)
                  	ARTin(Hs,i,1) = ARTin(Hs,i,1)-0.1
                	x=G3VOL(I,ARTin(:,:,1),IFIX,FIX)
                enddo
           	elseif(ARTin(Gs,i,1) == 0. .and. ARTin(Ns,i,1) > 0. .and. ARTin(Hs,i,1) > 0.)then
				Dia=DiaAtBRH(i,ARTin(Ns,i,1),ARTin(Hs,i,1))
                ARTin(Gs,i,1) = G3YTA(Dia,ARTin(Ns,i,1))
       			ARTin(Vs,i,1)= ARTin(Gs,i,1)*fh(i)
            endif
        enddo
! Timings
       	do i=1,NoOfSpecies
          BESTin(TotAge,1) = BESTin(TotAge,1)+(ARTin(BHAs,i,1)+T13(i,FIX(SI)))*ARTin(Gs,i,1)
        enddo
        BESTin(TotAge,1)=BESTin(TotAge,1)/(sum(ARTin(Gs,:,1))+0.001)
		BESTin(TimeCl,1)=BESTin(TotAge,1)		! Implicit assumption cleaning has not been done
		BESTin(TimeFe,1)=BESTin(TotAge,1)		! Implicit assumption fertilization has not been done
        if( BESTin(TotAge,1) >= FIX(LSA)-10. .and. sum(ARTin(Ns,:,1)) < 1000. )then
           BESTin(TimeTh,1)=30.		! Thinning < 35 y ago
        elseif( BESTin(TotAge,1) < FIX(LSA)-10. .and. sum(ARTin(Ns,:,1)) < 1000. )then
           BESTin(TimeTh,1)=10.		! Thinning < 15 y ago
        else
          BESTin(TimeTh,1)=BESTin(TotAge,1)		! Thinning > 35 y ago
        endif

!-- New forest --------------------------------------------------------------
    elseif(.not.OLDFOR)then
! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch <-- NB Bara tax/Heurekas arter
      	 READ(NR,*,END=99) NewFor, &
! Stems ha-1
        FNART(Ns,Pine,NewFor),FNART(Ns,Spruce,NewFor),FNART(Ns,Birch,NewFor),FNART(Ns,Aspen,NewFor), &
        FNART(Ns,Oak,NewFor),FNART(Ns,Beech,NewFor),FNART(Ns,SouthBrl,NewFor),FNART(Ns,Contorta,NewFor),&
        FNART(Ns,OtherBrl,NewFor),FNART(Ns,Larch,NewFor),FNART(Ns,HybAsp,NewFor),FNART(Ns,Poppel,NewFor), &
! Basal area (m2)
        FNART(Gs,Pine,NewFor),FNART(Gs,Spruce,NewFor),FNART(Gs,Birch,NewFor),FNART(Gs,Aspen,NewFor), &
        FNART(Gs,Oak,NewFor),FNART(Gs,Beech,NewFor),FNART(Gs,SouthBrl,NewFor),FNART(Gs,Contorta,NewFor), &
        FNART(Gs,OtherBrl,NewFor),FNART(Gs,Larch,NewFor),FNART(Gs,HybAsp,NewFor),FNART(Gs,Poppel,NewFor), &
! Height (m)
        FNART(Hs,Pine,NewFor),FNART(Hs,Spruce,NewFor),FNART(Hs,Birch,NewFor),FNART(Hs,Aspen,NewFor), &
        FNART(Hs,Oak,NewFor),FNART(Hs,Beech,NewFor),FNART(Hs,SouthBrl,NewFor),FNART(Hs,Contorta,NewFor), &
        FNART(Hs,OtherBrl,NewFor),FNART(Hs,Larch,NewFor),FNART(Hs,HybAsp,NewFor),FNART(Hs,Poppel,NewFor), &
! BHA (y)
        FNART(BHAs,Pine,NewFor),FNART(BHAs,Spruce,NewFor),FNART(BHAs,Birch,NewFor),FNART(BHAs,Aspen,NewFor), &
        FNART(BHAs,Oak,NewFor),FNART(BHAs,Beech,NewFor),FNART(BHAs,SouthBrl,NewFor),FNART(BHAs,Contorta,NewFor), &
        FNART(BHAs,OtherBrl,NewFor),FNART(BHAs,Larch,NewFor),FNART(BHAs,HybAsp,NewFor),FNART(BHAs,Poppel,NewFor)
        DomSpecies = maxloc(FNART(Ns,:,NewFor),1)
        do i=1,MXSPECI
            if(FNART(Gs,i,NewFor) == 0. .and. FNART(Ns,i,NewFor) > 0.)then
            	Dia=DiaYOUNG(i,0,FNART(:,:,NewFor),IFIX,FIX)
            	FNART(Gs,i,NewFor)= G3YTA(Dia,FNART(Ns,i,NewFor))
            endif
        enddo
        RETURN
    endif
    
!-- Skippa om ej med i domain
	if(NrDOM.ne.0)then
		do 22 i=1,NrDOM
22			if(IFIX(Domain).eq.GetDOM(i))go to 24
		    ierr=1
			RETURN
    endif

    !-- Prepare the state of period 1 ---------------------------------------------
24  IPER=1
    IFPER=IPER+1
	NRATG(IPER)=1
    x = year(iper)/5.
    BESTout(:,IPER)=BESTin(:,IPER)
    call G3ATB(IPER,NM,ARTin(:,:,IPER),BESTout(:,IPER),IFIX,FIX)
    ARTout(:,:,IPER)=ARTin(:,:,IPER)
    CALL  G3TVX(IPER)
    FORALL(J=1:NART,I = 1:MXSPECI) ARTin(J,I,IPER) = &
    ARTin(J,I,IFPER)*x + ARTout(J,I,IPER)*(1.-x)
    FORALL(I = 1:MXSPECI) ARTin(Ms,I,IPER) = ARTin(Ms,I,IFPER)    ! Mortality: Flow variable
    call G3ATB(IPER,NM,ARTin(:,:,IPER),BESTin(:,IPER),IFIX,FIX)
      
    BESTin(TotAge,IPER)=BESTin(TotAge,IPER)+year(iper)
    BESTin(TimeTh,IPER)=BESTin(TimeTh,IPER)+year(iper)
    BESTin(TimeCl,IPER)=BESTin(TimeCl,IPER)+year(iper)
    BESTin(TimeFe,IPER)=BESTin(TimeFe,IPER)+year(iper)
    
    if(m <= 100 .OR. MOD(m,1000) == 0)then
       write(*,'(i8)') M
    endif
    RETURN

! EOF
99    IEND=1

    RETURN
    END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine prepareStand(NR,NoNFI,Hkl,ierr,iend)
	USE G3_Global
    use G3_GAMAIN
	USE G3_GAPER
	USE G3_GAFRAM
	USE G3_GAATG
    USE G3_NAMES
    use G3_GANEWF
    USE G3_GAPER
	USE G3_interfaces
    use NFI_data
    INTEGER :: NR,NoNFI,Hkl,ierr,iend
    ! For stand data    
        integer      :: NoSt, &
                        St_Parcel, &
                        St_SoilMoist, &
                        St_Edge, &
                        St_Peat, &
                        i,j,k
        real         :: &
                        St_Area, &
                        St_Altitude, &
                        St_Latitude, &
                        St_Slope, &
                        St_TerrTrpDistance
        character    :: cin*600, &
                        cin2(40)*20, &
                        St_Stand*10, &
                        St_TerrTrpStatus*10,&
                        St_PlotID*13
    ! Dummy inläsning
       real :: x
       integer :: ix
       character :: c
       logical :: l

    ierr = 0
    iend = 0
    
    READ(NR,'(a)',END=99) cin
    i=1
    j=1
    k= 0
    do while(INDEX(cin(j:),',') > 0)
        k= INDEX(cin(j:),',')
        cin2(i)=cin(j:j+k-2)
        j = k + j
        i = i+1
		        enddo
    cin2(i)=cin(j:j+k-2)
    read(cin2(1),*)	   St_Parcel
    read(cin2(2),*)    St_Stand
    read(cin2(3),*)    St_PlotID
    read(cin2(4),*)    St_Area
    read(cin2(5),*)    x
    read(cin2(6),*)    x
    read(cin2(7),*)    c
    read(cin2(8),*)    St_Peat
    read(cin2(9),*)    ix
    read(cin2(10),*)   St_SoilMoist
    read(cin2(11),*)   St_Edge
    read(cin2(12),*)   St_Altitude
    read(cin2(13),*)   St_Latitude
    read(cin2(14),*)   ix
    read(cin2(15),*)   St_Slope
    read(cin2(16),*)   St_TerrTrpDistance
    read(cin2(17),*)   St_TerrTrpStatus

    !-- Transfer NFI data from storage to active plot -----------------------------
    j = 1
    do while(j < NoNFI)
        if( NFI_NFIid(j) == St_PlotID ) go to 10
        j = j +1
    enddo
    write(*,'(a)') '*** Error in G3GET: No NFI plot found'
    ierr = 1
    RETURN
    10      CFIX(StandID) = St_Stand
    FIX(Lat) = St_Latitude
    FIX(ASL) = St_Altitude
    if(St_TerrTrpStatus == 'OK')then
        FIX(mToRoad) = St_TerrTrpDistance
    else
       FIX(mToRoad) =  2000.		! Far from road or small
    endif
    IFIX(Domain) = St_Edge
    IFIX(LocCli) = NFI_LocCli(j)
    IFIX(Ftype) = NFI_Ftype(j)
    FIX(Tsum) = NFI_Tsum(j)
    if(St_Peat)then
       IFIX(Peat) = 1
    else
       IFIX(Peat) = 0
    endif
    IFIX(Ditch) = NFI_Ditch(j)
    IFIX(Moist) = NFI_Moist(j)
    IFIX(Zone) = NFI_Zone(j)
    FIX(SI) = NFI_SI(j)
    FIX(Slope) = St_Slope
    IFIX(Owner) = NFI_Owner(j)
    Hkl = NFI_Hkl(j)
    ARTin(Ns,:,1) = NFI_Stems(:,j)
    ARTin(Gs,:,1) = NFI_BA(:,j)
    ARTin(Hs,:,1) = NFI_Height(:,j) 
    ARTin(BHAs,:,1) = NFI_Age(:,j)
    ARTin(Vs,:,1) = NFI_Volume(:,j)

    return
99  iend = 1
    return
    
    end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11111
    subroutine prepareNFI(NR,NoNFI,Hkl,ierr,iend) 
	USE G3_Global
    use G3_GAMAIN
	USE G3_GAPER
	USE G3_GAFRAM
	USE G3_GAATG
    USE G3_NAMES
    use G3_GANEWF
    USE G3_GAPER
	USE G3_interfaces
    use NFI_data
    INTEGER :: NR,NoNFI,Hkl,ierr,iend
! Inläsning
    character :: Plot*13

    ierr = 0

    READ(NR,'(a)',END=99) Plot
    j=1
    do while(j < NoNFI)
        if( TRIM(NFI_NFIid(j)) == TRIM(Plot) ) go to 10
        j = j +1
    enddo
    write(*,'(a)') '*** Error in G3GET: No NFI plot found'
12  ierr = 1
    RETURN

10  CFIX(StandID) = NFI_NFIid(j)	! Beståndsnummer
    FIX(SI) = NFI_SI(j)
    if(FIX(SI) == 0)then
        ierr = 1
        return
    endif

     	    FIX(Lat) = NFI_Lat(j)			! breddgrad (o)
     	    FIX(ASL) = NFI_ASL(j)			! hÖ”jd ”ver havet (m)
     	    IFIX(Zone) = NFI_LocCli(j) 		! omr†lokalklimatiskt de(=0)
     	    IFIX(Ftype) = NFI_Ftype(j)		! skogstyp
      	    FIX(Tsum) = NFI_Tsum(j)		! Temperature sum
     	    IFIX(Peat) = NFI_Peat(j)
     	    IFIX(Ditch) = NFI_Ditch(j)
     	    IFIX(Moist) = NFI_Moist(j)		! Markfuktighet: 1 = Torr 2 = Frisk 3 = Frisk-fuktig 4 = Fukti
     	    IFIX(Zone) = NFI_Zone(j)		! geografisk zon (1= n:a Sv, 2= m:a Sv, 3= s:a Sv)
            FIX(mToRoad) = NFI_mToRoad(j)   ! stand/plot metres from road
            FIX(Slope) = NFI_Slope(j)		! code for slope
            IFIX(Owner) = NFI_Owner(j)			! code for owner
            Hkl = NFI_Hkl(j)		        ! Huggningsklass
            ARTin(Ns,:,1) = NFI_Stems(:,j)
            ARTin(Gs,:,1) = NFI_BA(:,j)
            ARTin(Hs,:,1) = NFI_Height(:,j) 
            ARTin(BHAs,:,1) = NFI_Age(:,j)
            ARTin(Vs,:,1) = NFI_Volume(:,j)

    return
99  iend = 1
    return
    
end
    
