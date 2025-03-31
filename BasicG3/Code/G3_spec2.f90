!----------------------------------------------------------------------
MODULE G3_Global
IMPLICIT NONE
SAVE
! Dimensioning parameters

INTEGER, PARAMETER :: MXPER=50		! Max no. of periods
INTEGER, PARAMETER :: MXATG=100		! Max no. of different treatments
INTEGER, PARAMETER :: MXNEWF=100	! Max no. of different types of new forest
INTEGER, PARAMETER :: MXVK=1000		! Max no. of conditions for treatments and new forests, respectively
INTEGER, PARAMETER :: MXVK1=MXVK+1	! 
INTEGER, PARAMETER :: MXDOM=50		! Max no. of domains (classes)
INTEGER, PARAMETER :: MXSPECI=12	! Max no. of species
INTEGER, PARAMETER :: NFIX=20       ! No. of elements in FIX and IFIX arrays
INTEGER, PARAMETER :: NBEST=20      ! No. of elements in BESTin and BESTout arrays
INTEGER, PARAMETER :: NART=10       ! No. of elements in ARTin and ARTout arrays
INTEGER, PARAMETER :: NCUT=4        ! No. of elements in CUT array
INTEGER, PARAMETER :: NTREAT=10     ! No. of treatment parameters in ATG array


END MODULE
!----------------------------------------------------------------------

!----------------------------------------------------------------------
MODULE G3_GANEWF
USE G3_Global
IMPLICIT NONE
SAVE

! Defines common variables NFNEW
INTEGER ::	NFNEW
REAL    ::  FNART(NART,MXSPECI,MXNEWF)
INTEGER ::	IOKNF1(MXVK1,MXNEWF)
INTEGER ::	IOKNF2(MXVK1,MXNEWF)
INTEGER ::	IOKNF3(MXVK1,MXNEWF)
REAL ::		OKNF(2,MXVK,MXNEWF)

END MODULE
!----------------------------------------------------------------------

!----------------------------------------------------------------------
MODULE G3_GAFRAM
USE G3_Global
IMPLICIT NONE
SAVE

! Defines common variables GAFRAM
CHARACTER(80) ::	CFIX(NFIX)
INTEGER ::	IFIX(NFIX)
REAL ::		FIX(NFIX)
REAL ::		BESTin(NBEST,MXPER),BESTout(NBEST,MXPER)
REAL ::		ARTin(NART,MXSPECI,MXPER),ARTout(NART,MXSPECI,MXPER)
INTEGER ::	NRATG(MXPER)
REAL ::		CUT(NCUT,MXSPECI,MXPER)

END MODULE
!----------------------------------------------------------------------

!----------------------------------------------------------------------
MODULE G3_GAATGC
USE G3_Global
IMPLICIT NONE
SAVE

! Defines common variables GAATGC
INTEGER ::	IOKA1(MXVK1,MXATG)
INTEGER ::	IOKA2(MXVK1,MXATG)
INTEGER ::	IOKA3(MXVK1,MXATG)
REAL ::		OKA(2,MXVK,MXATG)
INTEGER ::	IAUTA1(MXVK1,MXATG)
INTEGER ::	IAUTA2(MXVK1,MXATG)
INTEGER ::	IAUTA3(MXVK1,MXATG)
REAL ::		AUTA(2,MXVK,MXATG)

END MODULE
!----------------------------------------------------------------------

!----------------------------------------------------------------------
MODULE G3_GAATG
USE G3_Global
IMPLICIT NONE
SAVE

! Defines common variables GAATG
INTEGER ::	NATG
INTEGER ::	IXATG(MXATG)
REAL ::		ATG(NTREAT,0:MXSPECI,MXATG)
INTEGER ::	ISPEC_ATG(MXSPECI,MXATG)

END MODULE
!----------------------------------------------------------------------

!----------------------------------------------------------------------
MODULE G3_GAPER
USE G3_Global
IMPLICIT NONE
SAVE

! Defines common variables GAPER
INTEGER ::	NPER
INTEGER ::	IPER1
INTEGER ::	IPER2
INTEGER ::	IBREAK
REAL ::		YEAR(MXPER)

END MODULE
!----------------------------------------------------------------------

!----------------------------------------------------------------------
MODULE G3_GAMAIN
USE G3_Global
IMPLICIT NONE
SAVE

! Defines common variables GAMAIN
INTEGER ::	LROUT
INTEGER ::	LRSK
INTEGER ::	RUNFIL
INTEGER ::	RESFIL
INTEGER ::	LODFIL
INTEGER ::	ATGFIL
INTEGER ::	NRFIL(9)
INTEGER ::	MNO
INTEGER ::	NDUMP
INTEGER ::	NRLOAD
INTEGER ::	NOOUT1
INTEGER ::	IGEN
INTEGER ::	ID0
INTEGER ::	NCP
INTEGER ::	NRIN
INTEGER ::	NROUT
INTEGER ::	INDOM(MXDOM)
INTEGER ::	NDOM
CHARACTER :: VERSION*8

END MODULE
!----------------------------------------------------------------------

!----------------------------------------------------------------------
MODULE G3_NAMES	
IMPLICIT NONE
SAVE

!-- Names elements of array elements
!-- The function of the names is to facilitate reference to elements in the code
!-- The names are not referred to in the command file

! Spercies ART, FNART
INTEGER ::	&
    Pine,		& ! 1
    Spruce,		& ! 2 
    Birch,		& ! 3 
    Aspen,		& ! 4 
    Oak,		& ! 5 
    Beech,		& ! 6 
    SouthBrl,	& ! 7
    Contorta,	& ! 8 
    OtherBrl,	& ! 9 
    Larch,		& ! 10 
    HybAsp,		& ! 11 
    Poppel		  ! 12 
DATA Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch,HybAsp,Poppel	&
	/1,2,3,4,5,6,7,8,9,10,11,12/

! CFIX (use StandID)
! IFIX
INTEGER ::	StandID,Domain,Sapling,LocCli,Ftype,Peat,Ditch,Moist,Zone,CCscenario,Slope,Owner			
DATA StandID,Domain,Sapling,LocCli,Ftype,Peat,Ditch,Moist,Zone,CCscenario,Slope,Owner	&
	/1,2,3,4,5,6,7,8,9,10,11,12/
! FIX
INTEGER ::		Area,Spar,Lat,ASL,Tsum,SI,mToRoad								
DATA Area,Spar,Lat,ASL,Tsum,SI,mToRoad	&
	/1,2,3,4,5,6,7/

INTEGER ::  &               ! BEST
	TotAge,		& ! 1
    TimeTh,		& ! 2
    TimeCl,		& ! 3
    TimeFe,		& ! 4
    N,			& ! 5
    G,			& ! 6
    V,			& ! 7
    D,			& ! 8
    Hdom,		& ! 9
    PerFF,		& ! 10
    Mort,		& ! 13
    SpPin,		& ! 14
    SpSpr,		& ! 15
    SpDec,		& ! 16
    LSA,		& ! 17
    Geff1,		& ! 19
    Geff2		 ! 20
DATA TotAge,TimeTh,TimeCl,TimeFe,N,G,V,D,Hdom,PerFF,Mort,SpPin,SpSpr,SpDec,LSA,Geff1,Geff2	&
	/1,2,3,4,5,6,7,8,9,10,13,14,15,16,17,19,20/

INTEGER ::  &               ! ART
    Ns,     & ! 1
	N0s,	& ! 2
    Gs,     & ! 3
	Hs,		& ! 4
	BHAs,	& ! 5
    Vs,     & ! 6
    Ms        ! Mortality (m3/ha and 5 year)
DATA Ns,N0s,Gs,Hs,BHAs,Vs,Ms &
	/1,2,3,4,5,6,7/

INTEGER ::  &               ! CUT
	UVs,	& ! 1
	UNs,	& ! 2
	UDs,	& ! 3
	UHs	 	  ! 4
DATA UVs,UNs,UDs,UHs &
	/1,2,3,4/

INTEGER ::  &               ! ATG codes
	StrTh,	& ! 1
	GaTh,	& ! 2
	UN,		& ! 3
	NaTh,	& ! 4
    UDrel,	& ! 5
	FeType,	& ! 6
	FeKg,	& ! 7
    NFnr      ! 8
DATA	StrTh,GaTh,UN,NaTh,UDrel,FeType,FeKg,NFnr &
	/1,2,3,4,5,6,7,8/

INTEGER ::  &               ! Treatment codes
	NM,	& ! 1
	IP,	& ! 2
	Cl,		& ! 3
	Th,		& ! 4
	Fe,		& ! 5
	FT,	& ! 6
	FF		  ! 7
DATA NM,IP,Cl,Th,Fe,FT,FF &
	/1,2,3,4,5,6,7/
   

END MODULE
!----------------------------------------------------------------------
