      SUBROUTINE G3GET(NR,LREC,OLDFOR,M,NDOM,INDOM,IERR,IEND)
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
	USE G3_GAPER
	USE G3_GAFRAM
	USE G3_GAATG
    USE G3_NAMES
    use G3_GANEWF
    USE G3_GAPER
	USE G3_interfaces
save    
	LOGICAL :: OLDFOR
	INTEGER :: NR,LREC,M,INDOM(NDOM),IERR,IEND,IPER,IFPER
!-- EGNA DEKLARATIONER
	  INTEGER 	:: i,j,NoOfSpecies,DomSpecies,NewFor,Hkl
	  real 		:: hd,Rnd,RndLim,TBHA,s_2,roj,T13,HojdYoung,DiaYoung,DiaAtBRH
      LOGICAL	:: Start,Hugin,Brandel
      DATA NoOfSpecies/10/,Start/.true./,hd/10./,Hugin/.false./,Brandel/.false./                ! Övre höjd for inväxning (se RTVX)
! For volume computation of young stands
	real ::		fh(MXSPECI),NST,GY,VOLYM,VMEDEL,VSTAND,VSNED,VTOP,DomH,Vtot,Ntot,x
    DATA		fh/4.64,4.59,4.21,3.89,4.09,3.96,4.28,4.62,4.16,4.09,3.89,3.89/	! Form height from NFI 14-18, 7-10 m
    INTEGER :: 	its

      IERR=0
      IEND=0
!-- NOLLSTŽLLNING ------------------------------------------------------

	IFIX(:)	= 0
	FIX(:)		= 0.
	BESTin(:,:)	= 0.
	ARTin(:,:,:)	= 0.
    
    if(Start)then
 		write(*,'(a\)')'Rnd max limit: '
        read(*,*)RndLim
        Start = .false.
    endif

!-- Non stand values -------------------------------------------------
     	IFIX(CCscenario) = 1	! Climate change scenario: 0 = No growth effect; 1 = B2; 2 = A2
        IPER=1

!-- INLŽSNING AV VŽRDEN ------------------------------------------------

! SEKVENTIELL INLŽSNING

      IF(OLDFOR)THEN
       READ(NR,*,END=99) &
     	CFIX(StandID), &	! Beståndsnummer
     	IFIX(Domain), &		! Class
        Rnd, &				! Random number to select for test runs
        FIX(Area), &   		! Area
       x,x, &
     	FIX(Lat), &			! breddgrad (o)
     	FIX(ASL), &			! hÖ”jd ”ver havet (m)
     	IFIX(LocCli), & 		! omr†lokalklimatiskt de(=0)
     	IFIX(Ftype), &		! skogstyp
!           1= h”g”rt utan ris  2= l†g”rt utan ris  3= h”g”rt med bl†b„r
!           4=l†g”rt med bl†b„r  5= h”g”rt med ris utom bl†b„r
!           6= l†g”rt med ris utom bl†b„r  7= breda gr„s  8= smala gr„s
!           9= utan f„ltskikt  10= h”g starr  11= fr„ken (sumpskogstyp)
!           12= l†g starr  13= bl†b„r  14= lingon/mj”lon  15= kr†kb„r
!           16= ljung  17= odon/skvattram  18= rosling/tranb„r
      	FIX(Tsum), &		! Temperature sum
     	IFIX(Peat), &
     	IFIX(Ditch), &
     	IFIX(Moist), &		! Markfuktighet: 1 = Torr 2 = Frisk 3 = Frisk-fuktig 4 = Fukti
     	IFIX(Zone), &		! geografisk zon (1= n:a Sv, 2= m:a Sv, 3= s:a Sv)
       x,x,x, &
    	FIX(SI), &
        FIX(mToRoad), &		! stand/plot metres from road
        IFIX(Slope), &		! code for slope
        IFIX(Owner), &			! code for owner
        Hkl, &				! Huggningsklass
       x,x, &
    	(ARTin(Ns,i,1), i=1,NoOfSpecies), & ! Stems >= 5 cm
     	(ARTin(Gs,i,1), i=1,NoOfSpecies), & ! Basal area (m2)
     	(ARTin(Hs,i,1), i=1,NoOfSpecies), & ! Height (m)
     	(ARTin(BHAs,i,1), i=1,NoOfSpecies), & ! Age at brh (total age when read, modified below)
     	(ARTin(Vs,i,1), i=1,NoOfSpecies)  ! Volume (m3sk/ha)
! End read
      if(Rnd > RndLim)then
        	ierr = 1
            RETURN
        endif
 ! Prepare data
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


! New forest
      elseif(.not.OLDFOR)then
! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch,HybAsp,Poppel
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
        do i=1,NoOfSpecies
            if(FNART(Gs,i,NewFor) == 0. .and. FNART(Ns,i,NewFor) > 0.)then
            	Dia=DiaYOUNG(i,0,FNART(:,:,NewFor),IFIX,FIX)
            	FNART(Gs,i,NewFor)= G3YTA(Dia,FNART(Ns,i,NewFor))
            endif
        enddo
        RETURN
      ELSE
         STOP '*** Not implemented ***'
!         READ(NR,110,REC=M)IW1(1),IW1(2),W1(1),
!    *   (BESTin(I,1),I=1,4),(BESTin(I,1),I=10,12),
!    *   (ART(1,I,1),ART(3,I,1),ART(5,I,1),I=1,3)
      ENDIF
      
!-- Skippa om ej med i domain
	if(OLDFOR)then
		if(ndom.ne.0)then
			do 22 i=1,ndom
22				if(IFIX(Domain).eq.indom(i))go to 24
			ierr=1
			RETURN
		endif
    endif

! Prepare the state of period 1    
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
    
    write(*,'(i8,2a)')M,' ',CFIX(StandID)      
    RETURN

! EOF
99    IEND=1

    RETURN
    END

    
    !
!! Old code
!        if( sum(ARTin(Ns,:,1)) < 10. )then 	! Assume it is barren land if no or few trees
!          ARTin(:,:,1) = 0.0
!          BESTin(PerFF,1) = 1
!          BESTin(TotAge,1) = 0.
!        endif
!    	DomSpecies = MAXLOC(ARTin(Ns,:,1))
!		IF( IFIX(Sapling) == 1 .or. ARTin(Hs,DomSpecies,1) < hd )then
!          ARTin(Gs,:,1) =0.0 ! Make saplings or stands with height < hd to non established, .i.e. remove basal areas
!        else
!			do i=1,NoOfSpecies; 
!  				if(	ARTin(BHAs,i,1) > T13(FIX(Lat),FIX(SI)*10.,i)*2. )then
!              		ARTin(BHAs,i,1)= max(1.,ARTin(BHAs,i,1)-T13(FIX(Lat),FIX(SI)*10.,i))	! ålder i brösthöjd
!				else
!              		ARTin(:,i,1) = 0.0 ! Remove lagging trees on established plots/stands
!            	endif
!			enddo
!       	endif
!		BESTin(N,1)=SUM(ARTin(Ns,:,1))
!        BESTin(G,1)=SUM(ARTin(Gs,:,1))
!        if( BESTin(G,1) > 0. )then
!        	BESTin(TotAge,1) = sum(ARTin(BHAs,:,1)*ARTin(Gs,:,1))/(BESTin(G,1)+0.001) + T13(FIX(Lat),FIX(SI)*10.,DomSpecies)
!        else
!        	BESTin(TotAge,1) = sum(ARTin(BHAs,:,1)*ARTin(Ns,:,1))/(BESTin(N,1)+0.001)
!        endif
!		BESTin(TimeCl,1)=BESTin(TotAge,1)		! Implicit assumption cleaning has not been done
!		BESTin(TimeFe,1)=BESTin(TotAge,1)		! Implicit assumption fertilization has not been done
!        if( BESTin(TotAge,1) >= FIX(LSA)-10. .and. BESTin(N,1) < 1000. )then
!           BESTin(TimeTh,1)=30.		! Thinning < 35 y ago
!        elseif( BESTin(TotAge,1) < FIX(LSA)-10. .and. BESTin(N,1) < 1000. )then
!           BESTin(TimeTh,1)=10.		! Thinning < 15 y ago
!        else
!          BESTin(TimeTh,1)=BESTin(TotAge,1)		! Thinning > 35 y ago
!     	endif
!        do i=1,MXSPECI
!         	ARTin(Vs,i,1)= G3VOL(I,ARTin(:,:,1),IFIX,FIX)
!          	if(ARTin(Gs,i,1) == 0.)ARTin(Gs,i,1) = ARTin(Vs,i,1)/fh(i)
!        enddo
!        call G3ATB(IPER,NM,ARTin(:,:,IPER),BESTin(1,IPER),IFIX,FIX)
!! End old code
