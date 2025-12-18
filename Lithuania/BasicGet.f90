MODULE Lt_GrowthData
USE G3_Global
IMPLICIT NONE
SAVE
! Keep relative growth figures

REAL    :: RelGr(NART,MXSPECI,MXPER)
REAL    :: OrigonalARTin(NART,MXSPECI,MXPER)

END MODULE

    
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
    use Lt_GrowthData
	USE G3_interfaces
save    
	LOGICAL :: OLDFOR
	INTEGER :: NR,LREC,M,INDOM(NDOM),IERR,IEND
!-- EGNA DEKLARATIONER
    INTEGER  :: StartOld,StartNew,IPER,IFPER,SpOld,SpNew
    INTEGER  :: nratgNew(MXPER),nratgOld(MXPER)
    REAL        :: Dx,BHANew,NsNew,GsNew,VsNew,HsNew
    LOGICAL	:: Start,EndRead
    character   :: Cstart,StandIDOld*80,StandIDNew*80
    DATA NoOfSpecies/10/,Start,EndRead/.true.,.false./

      IERR=0
      IEND=0
!-- NOLLSTŽLLNING ------------------------------------------------------

	CFIX(:)	= ' '
	IFIX(:)	= 0
	FIX(:)		= 0.
	BESTin(:,:)	= 0.
	ARTin(:,:,:)	= 0.
    
    if(Start)then
        read(nr,*,end=99)CFIX(StandID),StartNew,iper,nratgNew(iper),SpNew, &
     	ARTin(BHAs,SpNew,iper), & ! Age at brh (total age when read, modified below)
        ARTin(Ns,SpNew,iper), & ! Stems >= 5 cm
     	ARTin(Gs,SpNew,iper), & ! Basal area (m2)
      	ARTin(Vs,SpNew,iper), &  ! Volume (m3sk/ha)
        Dx, &
  	    ARTin(Hs,SpNew,iper) ! Height (m)
        Start = .false.
    elseif(EndRead)then
        IEND =1
        return
    else
        iper = 1
        CFIX(StandID) = StandIDNew
     	ARTin(BHAs,SpNew,iper) = BHANew ! Age at brh (total age when read, modified below)
        ARTin(Ns,SpNew,iper) = NsNew ! Stems >= 5 cm
     	ARTin(Gs,SpNew,iper) = GsNew ! Basal area (m2)
      	ARTin(Vs,SpNew,iper) = VsNew  ! Volume (m3sk/ha)
  	    ARTin(Hs,SpNew,iper) = HsNew ! Height (m)
    endif
    StandIDOld = CFIX(StandID)
    StandIDNew = StandIDOld
    StartOld = StartNew
    SpOld = SpNew
    nratgOld(:) = nratgNew(:)

! Read one stand
    do while(StandIDOld == StandIDNew .AND. SpOld == SpNew .AND. .not.EndRead )
        read(nr,*,end=99)StandIDNew,StartNew,ipernew,nratgNew(ipernew),SpNew, &
     	BHANew, & ! Age at brh (total age when read, modified below)
        NsNew, & ! Stems >= 5 cm
     	GsNew, & ! Basal area (m2)
      	VsNew, &  ! Volume (m3sk/ha)
        Dx, &
  	    HsNew ! Height (m)
        if(StandIDOld == StandIDNew .AND. SpOld == SpNew)then
            iper = ipernew
            CFIX(StandID) = StandIDNew
     	    ARTin(BHAs,SpNew,iper) = BHANew ! Age at brh (total age when read, modified below)
            ARTin(Ns,SpNew,iper) = NsNew ! Stems >= 5 cm
     	    ARTin(Gs,SpNew,iper) = GsNew ! Basal area (m2)
      	    ARTin(Vs,SpNew,iper) = VsNew  ! Volume (m3sk/ha)
  	        ARTin(Hs,SpNew,iper) = HsNew ! Height (m)
        endif
    enddo
    go to 24
    ! EOF
99    EndRead=.true.

! Prepare "growth" projection data    
24 NPER=iper
! Register relative change of forest data
   do iper = 1,NPER-1
       do i = 1,MXSPECI
           RelGr(Ns,i,iper) = ARTin(Ns,i,iper+1)/(ARTin(Ns,i,iper)+0.001)
           RelGr(Gs,i,iper) = ARTin(Gs,i,iper+1)/(ARTin(Gs,i,iper)+0.001)
           RelGr(Vs,i,iper) = ARTin(Vs,i,iper+1)/(ARTin(Vs,i,iper)+0.001)
           RelGr(Hs,i,iper) = ARTin(Hs,i,iper+1)/(ARTin(Hs,i,iper)+0.001)
       enddo
    enddo
   do iper = 1,NPER
       do i = 1,MXSPECI
           OrigonalARTin(Ns,i,iper) = ARTin(Ns,i,iper)
           OrigonalARTin(Gs,i,iper) = ARTin(Gs,i,iper)
           OrigonalARTin(Vs,i,iper) = ARTin(Vs,i,iper)
           OrigonalARTin(Hs,i,iper) = ARTin(Hs,i,iper)
       enddo
    enddo
           
    BESTin(TotAge,1)=MAXVAL(ARTin(BHAs,:,1))
    BESTin(TimeTh,1)=BESTin(TotAge,1)
    BESTin(TimeCl,1)=BESTin(TotAge,1)
    BESTin(TimeFe,1)=BESTin(TotAge,1)
    call G3ATB(1,NM,ARTin(:,:,1),BESTin(:,1),IFIX,FIX)
    
    LREC = StartOld ! Spec Lithuania
    if(ENDRead)then
        NRATG(1:NPER) = nratgNew(1:NPER)
    else
        NRATG(1:ipernew) = nratgOld(1:ipernew)
        NRATG(ipernew+1:NPER) = nratgNew(ipernew+1:NPER)
    endif
    
    write(*,'(i8,2a,i4)')M,' ',TRIM(CFIX(StandID)),LREC 
    
    RETURN
    END

    SUBROUTINE G3GET_NewForest(NR,IERR,IEND)
!***********************************************************************
! Reads in then state forest to begin established on barren land, 
! existing OR created by final felling.
! The state forest data are stored in FNART array. What forest in FNART 
! that should be inplanted, and in what state of the forest,
! is defined as for other actions DEFA and OKACT.
! The file opened with number NR is always read to end.
!  PARAMETRAR:
!     NR    = (in) logiskt logical number of the reading channel
!     IERR  = (ut) something went wrongs
!     IEND  = (ut) something went wrongs
!***********************************************************************
!-- GAYA-DEKLARATIONER
	USE G3_Global
    USE G3_NAMES
    use G3_GANEWF
save    
	INTEGER :: NR,IERR,IEND
!-- EGNA DEKLARATIONER
   INTEGER, PARAMETER :: NoOfSpecies = 6

   READ(NR,*,END=99) NewFor, &
! Stems ha-1
        FNART(Ns,1:NoOfSpecies,NewFor), &
! Basal area (m2)
        FNART(Gs,1:NoOfSpecies,NewFor), &
! Height (m)
        FNART(Hs,1:NoOfSpecies,NewFor), &
! BHA (y)
        FNART(BHAs,1:NoOfSpecies,NewFor)

        return
        
99      IEND = 1
        return
END
