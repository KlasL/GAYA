Module G3_interfaces

    Interface
!==========================================================================================
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
	USE G3_GAFRAM
	USE G3_GANEWF
    USE G3_NAMES
    USE G3_GAATG
	LOGICAL :: OLDFOR
	INTEGER :: NR,LREC,M,INDOM(NDOM),IERR,IEND
	End subroutine
!==========================================================================================
	SUBROUTINE G3OUT(NR,M,IPER,IREC)
! USER SPECIFIC
!***********************************************************************
!  Rutin f”r utskrift av alternativ. Kallas efter  om IERR i denna
!  rutin lika med 0.
!
!  PARAMETRAR:
!     NR    = (in) logiskt nummer (”ppnas mot fil med kommandot FOUT)
!     M     = (in) ordningsnummer p† best†nd/yta f”r vilket ber„kning
!                  skett (svarar mot parameter M i rutin G3GET)
!     IPER  = (in) sista perioden för vilken ber„kningar skett
!     IREC  = (ut) antal records som skrivs ut i rutinen (normalt=1)
!***********************************************************************
!-- GAYA-DEKLARATIONER
	USE G3_Global
	USE G3_GAPER
	USE G3_GAATG
	USE G3_GAATGC
	USE G3_GAFRAM
    USE G3_NAMES
    INTEGER :: NR,M,IPER,IREC
	End subroutine
!==========================================================================================
      SUBROUTINE G3INIT
! GAYA SPECIFIC      
!***********************************************************************
! logical number of files associated with different commands
! must be called before opening command file (channel NR 72)
!***********************************************************************
	USE G3_Global
	USE G3_GAMAIN
	End subroutine
!==========================================================================================
      SUBROUTINE G3COMD
!! GAYA SPECIFIC      
!!***********************************************************************
!! Interprets the content of file the command file (opened with channel 72)
!!
!!  PARAMETRAR:
!!     ISTART   = (in) 1 = first call; 0 = repeted call 
!!                (ut) 0
!!***********************************************************************
	USE G3_Global
	USE G3_GANEWF
	USE G3_GAMAIN
	USE G3_GAPER
	USE G3_GAATG
	USE G3_GAATGC
	USE G3_GAFRAM
	End subroutine
!==========================================================================================
      SUBROUTINE G3JFR(IC1,COMM,IC2,CC,NR)
! GAYA SPECIFIC      
!***********************************************************************
! Returns the number of the substring in COMM that corresponds to string CC.
! CC may be shortened, should be left adjusted.
!
!        IN: COMM = String /SUBSTR1 SUBSTR2..../
!            CC   = String which is compared to subsrings of COMM
!
!        UT: NR   = CC is blank or there is no correspondence with COMM
!                   CC correspondence with several substrings of COMM => <0
!                   CC corresponds with one substrings of COMM => NR=SUBSTRNR
!***********************************************************************
INTEGER, INTENT(IN)                      :: ic1
CHARACTER (LEN=ic1), INTENT(IN)          :: comm
INTEGER, INTENT(IN)                      :: ic2
CHARACTER (LEN=ic2), INTENT(IN)          :: cc
INTEGER, INTENT(OUT)                     :: nr
	End subroutine
!==========================================================================================
      SUBROUTINE G3FRAM(ICONT,NDP,IATG,IPER,IERR)
! GAYA SPECIFIC      
!***********************************************************************
!  Routine for administrating the projectio of a stand/plot.
! The first step is to call for transferring the in-state to out-state in 
! the same period by calling G3ATG(IPER,ID,IERR).
! The next step is to call one of the routines 
! G3ETVX(IPER), G3RTVX(IPER), and G3INPL(IPER) to transfer the state 
! from period IPER to the next period.
!
!  PARAMETRAR:
!     ICONT    = (in) period for which projection is ot begin
!     NDP      = (in) /not used/
!     IATG     = (in) treatment numbers for each period 
!     IPER     = (ut) last projection period or period with non accepted treatment (IERR =1) 
!     IERR     = (ut) 1 => prescribed treatment not accepted, else 0
!***********************************************************************
	USE G3_Global
	USE G3_GAPER
	USE G3_GAATG
	USE G3_GAFRAM
    USE G3_NAMES
     INTEGER ::  ICONT,NDP,IATG(MXPER),IPER,IERR
 	End subroutine
!==========================================================================================
      SUBROUTINE G3ATG(IPER,ID,IERR)
! GAYA SPECIFIC      
!***********************************************************************
! Step 1: test if the treatment is accepted
! Step 2: calls G3IN2OUT to transferring the in-state to out-state in 
! the same period
!
!  PARAMETRAR:
!     IPER  = (in) current period
!     ID    = (in) treatment number
!     IERR  = (ut) 1 => prescribed treatment not accepted, else 0
!***********************************************************************
	USE G3_Global
	USE G3_GAATG
	USE G3_GAATGC
	USE G3_GAFRAM
    USE G3_NAMES
	INTEGER ::	IPER,ID,IERR
	End subroutine
!==========================================================================================
      SUBROUTINE G3IN2OUT(ID,IPER,IERR)
! USER SPECIFIC
!***********************************************************************
!  Transfers the in-state to out-state in the same period after treatment (incl. undisturbed growth)
!
!  PARAMETRAR:
!     ID    = (in) treatment number
!     IPER  = (in) current period
!     IERR  = (ut) 1 => inconsistent description of treatment, else 0
!***********************************************************************
	USE G3_Global
	USE G3_GAATG
	USE G3_GAFRAM
	INTEGER ::	ID,IPER,IERR
	End subroutine
!==========================================================================================
      INTEGER FUNCTION IG3CHK(IXY1,IXY2,IXY3,XY,IPER)
! GAYA SPECIFIC      
!***********************************************************************
! Investigates with Boolean algebra whether a condition is fullfille or not.
! Conditions are based on groups consisting of triplet
! (variabel, lower limit, upper limit) of closed interval.
! Groups are connected with AND conditions (= 1) and OR conditions (= 0).
! Variables belong to (selection from and defined by the call) 
! BEST, FIX, IFIX, and VAR and PERIOD number.
!
!  PARAMETRAR OCH FUNKTIONSVŽRDE:
!     IG3CHK = 1 => conditions fullfilled, else 0
!     IXY1   = variabels whose value should be investigated
!     IXY2   = connection with 1 for AND and 0 for OR
!     IXY3   = species code; 0= variable in BEST; #0 = species in ART
!     XY(1,.)= lower limit
!     XY(2,.)= upper limit
!     IPER   = current period
!***********************************************************************
	USE G3_Global
	USE G3_GAFRAM
	INTEGER :: IXY1(MXVK1),IXY2(MXVK1),IXY3(MXVK1),IPER
	REAL	:: XY(2,MXVK)
	End function
!==========================================================================================
      REAL FUNCTION G3DIA(GRYTA,STAM)
! GAYA SPECIFIC      
!***********************************************************************
! Computes the basal area weighted diameter 
!
!  PARAMETERAR:
!     G3DIA (ut)    = basal area weighted diameter (cm)
!     GRYTA (in)    = basal area (m2/ha)
!     STAM (in)     = number of stems (/ha)
!***********************************************************************
	REAL	:: GRYTA,STAM
	End function
!==========================================================================================
      REAL FUNCTION G3YTA(DIA,STAM)
! GAYA SPECIFIC      
!***********************************************************************
!  Computes the basal area
!
!  PARAMETERAR:
!     G3YTA    = basal area (m2/ha)
!     DIA      = basal area weighted diameter (cm)
!     STAM     = number of stems (/ha)
!***********************************************************************
	REAL	:: DIA,STAM
	End function
!==========================================================================================
      SUBROUTINE G3TVX(IPER)
! USER SPECIFIC
!***********************************************************************
! Computes the state of the forest for period PERIOD + 1 going from 
! BESTout(IPER) and ARTout(IPER+1) to BESTin(IPER) and ARTin(IPER+1)
! for established forest, i.e. BESTout(G,IPER) > 0.
!
!  PARAMETRAR:
!     IPER  = (in) current period
!  Some data used in growth projection:
!     ID    = treatment number
!     ATG   = treatment definition for treatment ID
!     CUT   = harvest data
!     IFIX  = fixed (INTEGER) data
!     FIX   = fixed (REAL) data
!***********************************************************************
!-- GAYA-DEFINITIONER
	USE G3_Global
    USE G3_GAFRAM
	USE G3_NAMES
    INTEGER	:: IPER
	End subroutine
!==========================================================================================
	REAL FUNCTION G3VOL(I,ART,IFIX,FIX)
!***********************************************************************
!  Static volume assessment for species no. I
!***********************************************************************
 	USE G3_Global
    USE G3_NAMES
    INTEGER :: IFIX(NFIX)
    REAL    :: FIX(NFIX),ART(NART)
	End function
!==========================================================================================
	REAL FUNCTION G3HOJD(I,ART,IFIX,FIX)
!***********************************************************************
!  Height at total at age A1+5 based on existing height at age A1
!***********************************************************************
 	USE G3_Global
    USE G3_NAMES
    INTEGER :: I,IFIX(NFIX)
    REAL    :: FIX(NFIX),ART(NART,MXSPECI)
	End function
!==========================================================================================
     SUBROUTINE G3ATB(IPER,IXATG,ART,BEST,IFIX,FIX)
!***********************************************************************
!  Come here to compute:
!   - non-dynamic ART variables
!   - BEST treatment related variables
!   - BEST variables as a function of ART
!***********************************************************************
	USE G3_Global
    USE G3_NAMES
    INTEGER :: IPER,IXATG,IFIX(NFIX)
    REAL	:: ART(NART,MXSPECI),BEST(NBEST),FIX(NFIX)
	End subroutine
!==========================================================================================
    real function dfh(art)
  	USE G3_Global
    USE G3_NAMES
    real    :: art(nart)
	End function
!==========================================================================================
    SUBROUTINE Thin(ATG,ARTin, Nx,Gx,RD,IERR)
!***********************************************************************
! Come here to execute (precommercial) thinning for iies i
! ATG(:)		= the treatment parameters
! ARTIN(:)	= the state of the species or remaining state of the stand
!
! Nx			= (out) the share of stems to be harvested
! Gx			= (out) the share of basal area and volume to be harvested
! RD			= (out) relative diameter of harvested trees
!***********************************************************************
    USE G3_Global
    USE G3_NAMES

    INTEGER	:: IERR
	REAL    :: ATG(NTREAT),ARTin(NART),Nx,Gx,RD
	End subroutine
!==========================================================================================

	End Interface

End Module


