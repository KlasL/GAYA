SUBROUTINE g3init

! Code converted using TO_F90 by Alan Miller
! Date: 2024-02-09  Time: 18:37:53

!***********************************************************************
! SETS NUMBERS ON LOGICAL UNITS. MUST BE GIVEN BEFORE OPENING
! COMMAND FILE (72)
!***********************************************************************
use g3_global
use g3_gamain
use g3_interfaces

! LOGICAL UNITS
nrfil( 1) = 65 ! FOUT
nrfil( 2) = 66 ! FNEWFOREST
nrfil( 3) = 67 ! FFOREST
nrfil( 4) = 68 ! FRUN
nrfil( 5) = 69 ! FNORUN
nrfil( 6) = 70 ! FRESOURCE
nrfil( 7) = 71 ! FLOAD
nrfil( 8) = 72 ! FCOMMAND
nrfil( 9) = 73 ! FACTION
RETURN
END SUBROUTINE g3init

SUBROUTINE g3comd
!***********************************************************************
! INTERPRETS THE CONTENT OF THE COMMAND FILE (OPENED WITH NR 72)

!  ANROPADE RUTINER: G3JFR, G3GET
!***********************************************************************
!-- COMMON DEFINITIONS
use g3_global
use g3_ganewf
use g3_gamain
use g3_gaper
use g3_gaatg
use g3_gaatgc
use g3_gafram
use g3_interfaces
SAVE
!
INTEGER :: istart,iskip
INTEGER :: i,j,k

!-- LOCAL DEFINITIONS
REAL :: ta,tb,tused
CHARACTER (LEN=11) :: com2(32)*11
CHARACTER (LEN=352) :: comm*352
CHARACTER (LEN=122) :: cvar
CHARACTER (LEN=23) :: catg
CHARACTER (LEN=20) :: ctrt,chin*256,ch*10000,namn*256,ctemp*10

data istart/1/

!-- FIXA DATA

DATA com2( 1)/'FOUT       '/, com2( 2)/'FNEWFOREST '/,  &
    com2( 3)/'FFOREST    '/, com2( 4)/'FRUN       '/,  &
    com2( 5)/'FNORUN     '/, com2( 6)/'FRESOURCE  '/,  &
    com2( 7)/'FLOAD      '/, com2( 8)/'FCOMMAND   '/,  &
    com2( 9)/'FACTION    '/
DATA com2(10)/'DEFACTION  '/, com2(11)/'OKACTION   '/,  &
    com2(12)/'OKNEWFOR   '/, com2(13)/'AUTACTION  '/,  &
    com2(14)/'NOTOUT1    '/, com2(15)/'OUT1       '/,  &
    com2(16)/'LOG        '/, com2(17)/'NOLOG      '/,  &
    com2(18)/'GENMETHOD  '/, com2(19)/'NPER       '/
DATA com2(20)/'xxx        '/, com2(21)/'P1ACTION   '/,  &
    com2(22)/'PINTACTION '/, com2(23)/'BREAKACTIN '/,  &
    com2(24)/'PAVAR      '/, com2(25)/'RUN        '/,  &
    com2(26)/'STOP       '/, com2(27)/'STATISTICS '/,  &
    com2(28)/'CLASSACTIO '/, com2(29)/'YEARS      '/
DATA com2(30)/'VERSION    '/, com2(31)/'SKIP       '/,  &
     com2(32)/'UNSKIP     '/
DATA ctrt/'NM IP Cl Th Fe FT FF'/
DATA catg/'TS BA SR SA RD FA FY NF'/
!                   1  2  3  4  5  6  7  8  9 10 11 12
DATA cvar( 1: 30)/'TA TT TP TF ST BA VO DI HE X1 '/		! BEST
DATA cvar(31: 60)/'X2 X3 X4 SP SS SD LS X5 X6 X7 '/		! BEST
DATA cvar(61: 96)/'I1 I2 I3 LC FT PE DH MO ZO I4 I5 I6 '/ 	! IFIX
DATA cvar(97:122)/'F1 F2 LT AS TS SI F3 PR'/	! FIX + Period
DATA nritot,nrotot,LOG,tused/4*0/

!-- INITIERING
IF(istart == 1)THEN
    j = 1
    do i = 1,32
        do k = 1,11
            COMM(j:j) = com2(i)(k:k)
            j = j+1
        enddo
    enddo
! DEFAULT VALUES
  LOG      = 0
  iskip    = 0
  nfnew    = 0
  natg     = 0
  ixatg    = 0
  runfil   = 0
  resfil   = 0
  lodfil   = 0
  atgfil   = 0
  noout1   = 0
  igen     = 0
  id0      = 0
  ibreak   = 0
  mno      = 999999
  nper     = mxper
  tpush    = 0.
  tlength  = 5.
  ncp      = mxper
  iper1    = 1
  iper2    = 1
  ioka1    = 0
  ioka2    = 0
  ioka3    = 0
  iauta    = 0
  ioknf    = 0
  indom    = 0
  ndom     = 0
  version  = ' '
  nr       = 0
  fnart    = 0.
  ispec_atg= 0
  atg      = 0.
  forall(i=1:mxper)year(i)=(i-1)*tlength
END IF

!-- STATISTICS --------------------------------------------------------

IF(istart /= 0)THEN
  CALL cpu_time(ta)
  tb=ta
  istart=0
ELSE
  nritot=nritot+nrin
  nrotot=nrotot+nrout
END IF

!-- READ, INTERPRET AND PREPARE COMMAND ------------------------------

icont=0
1 READ(nrfil(8),'(A)',END=260)chin
IF(LOG == 1 .AND. iskip == 0)WRITE(*,*)trim(chin(1:))
IF(chin(1:1) == '*')GO TO 1
IF(icont == 0)THEN
  chin=adjustl(chin)
  CALL  g3jfr(352,comm,INDEX(chin,' '),chin,nr)
  IF(nr == 0)THEN
    WRITE(*,*)' *** NO DEFINED COMMAND: ', chin(1:INDEX(chin,' ')),' ***'
    GO TO 1
  ELSE IF(nr < 0)THEN
    WRITE(*,*)' *** NO UNIQUELY DEFINED COMMAND: ',  &
        chin(1:INDEX(chin,' ')),' ***'
    GO TO 1
  END IF
  chin=chin(INDEX(chin,' '):256)
  nz=0
END IF

!-- REMOVE BLANKS AND TABS IN PARAMETER LIST

j=0
DO i=1,256
  IF(chin(i:i) /= ' '.AND.iachar(chin(i:i)) /= 9.AND.chin(i:i) /= '"')THEN
    j=j+1
    chin(j:j)=chin(i:i)
  END IF
END DO


!C-- DETERMINE ACTION DEPENDENT ON IF PREVIOUS OR CURRENT ROW
!-- HAS CONTINUATION OR NOT

IF(icont == 1)THEN
  ch=ch(1:nz)//chin
ELSE
  ch=chin
END IF

nz=nz+j
IF(nz > 10000)THEN
  WRITE(*,*) ' *** Error: No. of characters in command exceeds 10000 ***'
  STOP
END IF

IF(INDEX(ch,'&') /= 0)THEN
  icont=1
  nz=INDEX(ch,'&')-1
  GO TO 1
ELSE
  icont=0
  ch(nz+1:)=' '
END IF

!-- CHOOSE EXECUTION STEP

IF(iskip == 1 .AND. nr /= 32)GO TO 1
GO TO(      5,  5,  5,  5,  5, 5,  5,  5,  5,  &
    100,110,120,130,140,150,160,170,180,190,  &
    200,210,220,230,240,250,260,270,280,290,300,310,320),nr

!***********************************************************************
!********************* EXECUTION ***************************************
!***********************************************************************

!-- FILE OPENING AND CLOSING

5   IF(nz == 0)THEN
        CLOSE(nrfil(nr))
        goto 1
    ELSE
        READ(ch(1:nz),'(a)')namn(1:nz)
        OPEN(nrfil(nr),FILE=namn(1:nz),ERR=6)
    END IF

SELECT CASE ( nr )
  CASE (    1)
    GO TO 10
  CASE (    2)
    GO TO 20
  CASE (    3)
    GO TO 30
  CASE (    4)
    GO TO 40
  CASE (    5)
    GO TO 50
  CASE (    6)
    GO TO 60
  CASE (    7)
    GO TO 70
  CASE (    8)
    GO TO 80
  CASE (    9)
    GO TO 90
END SELECT

6     WRITE(*,*)' *** ERROR OPENING FILE:',namn
GO TO 1

!-- FOUT ---------------------------------------------------------------

10    lrout=0
GO TO 1

!-- FNYSK --------------------------------------------------------------

20    nfnew=0
IF(nz == 0)GO TO 1
k=0
21      k=k+1
i=0
CALL  g3get(nrfil(2),lr,.false.,k,1,indom,ierr,iend)
IF(iend == 1)GO TO 1
IF(ierr == 1)GO TO 21
nfnew=nfnew+1
IF(nfnew > mxnewf)THEN
  WRITE(*,*)' *** NYSK - MORE THAN',mxnewf,' PROGRAMS SPECIFIED'
  nfnew=mxnewf
  GO TO 1
END IF
GO TO 21

!-- FSKOG --------------------------------------------------------------

30    lrsk=lr
read(nrfil( 3),*)	! Read header
m=0
GO TO 1

!-- FRUN ---------------------------------------------------------------

40    runfil=1
IF(nz == 0)runfil=0
GO TO 1

!-- FNORUN -------------------------------------------------------------

50    mno=0
IF(nz == 0)mno=999999
GO TO 1

!-- FRESOURCE ---------------------------------------------------------

60    resfil=1
GO TO 1

!-- FLOAD --------------------------------------------------------------

70    CONTINUE
GO TO 1

!-- FSTYR --------------------------------------------------------------

80    CONTINUE
GO TO 1

!-- FATG ---------------------------------------------------------------

90    atgfil=1
IF(nz == 0)atgfil=0
GO TO 1

!-- DEFATG -------------------------------------------------------------

! TREATMENT NUMBER K
100   READ(ch,*)k
natg=MAX(natg,k)
IF(natg > mxatg)THEN
  WRITE(*,'(A,I4)')' DEFATG - No. of treatment exceeds ',mxatg
  GO TO 260
END IF
atg(1:10,0:mxspeci,k)=0.0
nk=INDEX(ch,',')
IF(nk == 0)THEN
  ixatg(k)=0
  GO TO 1
END IF
CALL g3jfr(20,ctrt,2,ch(nk+1:nk+2),j)
IF(j <= 0)THEN
  WRITE(*,*)' DEFATG - Wrong def. of treatment:',ch(nk+1:nk+2)
  GO TO 260
ELSE
  ixatg(k)=j
END IF
nk=INDEX(ch(nk+1:),',')
! FIND OUT NUMBER OF SPECIES AND NUMBER THEM
nspec=0
ispec_atg(1:mxspeci,k)=0
101     IF(INDEX(ch(nk+1:),'(') /= 0)THEN
  i =nk+INDEX(ch(nk+1:),'(')
  nk=nk+INDEX(ch(nk+1:),')')
  ctemp=ch(i+1:nk-1)
  READ(ctemp,*)j
  DO i=1,nspec
    IF(ispec_atg(i,k) == j)GO TO 101
  END DO
  nspec=nspec+1
  IF(nspec > mxspeci)THEN
    WRITE(*,'(A,i4)')' DEFATG - No. of species exceeds ',mxspeci
    GO TO 260
  END IF
  ispec_atg(nspec,k)=j
  GO TO 101
END IF
! SPEC OF TREATMENT: NR(SP)=XX
nk=INDEX(ch,',')
102     IF(INDEX(ch(nk+1:),'=') == 0)GO TO 1
nk=nk+INDEX(ch(nk+1:),'=')
IF(ch(nk-1:nk-1) == ')')THEN
  i1=INDEX(ch(1:nk),'(',.true.)+1
  ctemp=ch(i1:nk-2)
  READ(ctemp,*)nspec
  i1=i1-3
ELSE
  nspec=0
  i1=nk-2
END IF
CALL g3jfr(23,catg,2,ch(i1:i1+1),j)
IF(j <= 0)THEN
  WRITE(*,*)' DEFATG - Wrong def. of treatment:',ch(nk-2:nk-1)
  GO TO 260
ELSE
  READ(ch(nk+1:),*)atg(j,nspec,k)
END IF
GO TO 102

!-- OKATG --------------------------------------------------------------

! TREATMENT NUMBER K
110     READ(ch,*)k
ioka1(1:mxvk1,k)=0; ioka2(1:mxvk1,k)=0
nk=INDEX(ch,',')
IF(nk == 0)THEN
  WRITE(*,*)' OKATG - Wrong specification: ',trim(ch)
  GO TO 260
END IF
nvk=0
112     IF(INDEX(ch(nk+1:),'=') == 0)THEN
  ioka1(nvk+1,k)=0
  GO TO 1
END IF
nk=nk+INDEX(ch(nk+1:),'=')
! NEW CONDITION
nvk=nvk+1
IF(nvk > mxvk)THEN
  WRITE(*,*)' OKATG - Too many conditions. Max is ',mxvk
  GO TO 260
END IF
! VARIABLE AND (MAYBE) SPECIES
IF(ch(nk-1:nk-1) == ')')THEN
  i1=INDEX(ch(1:nk),'(',.true.)+1
  ctemp=ch(i1:nk-2)
  READ(ctemp,*)ioka3(nvk,k)
  i1=i1-3
ELSE
  ioka3(nvk,k)=0
  i1=nk-2
END IF
CALL  g3jfr(122,cvar,2,ch(i1:i1+1),ioka1(nvk,k))
IF(ioka1(nvk,k) <= 0)THEN
  WRITE(*,*)' OKATG - Wrong def. of variabel: ',ch(nk-2:nk-1)
  GO TO 260
END IF
! CONNECTION TO NEXT CONDITION (* = AND; + = OR)
IF(INDEX(ch(nk+1:),'*')+INDEX(ch(nk+1:),'+') == 0)THEN
  i=nz-nk
  ioka2(nvk,k)=0
ELSE IF(INDEX(ch(nk+1:),'*') == 0)THEN
  i=INDEX(ch(nk+1:),'+')-1
  ioka2(nvk,k)=0
ELSE IF(INDEX(ch(nk+1:),'+') == 0)THEN
  i=INDEX(ch(nk+1:),'*')-1
  ioka2(nvk,k)=1
ELSE IF(INDEX(ch(nk+1:),'+') < INDEX(ch(nk+1:),'*'))THEN
  i=INDEX(ch(nk+1:),'+')-1
  ioka2(nvk,k)=0
ELSE
  i=INDEX(ch(nk+1:),'*')-1
  ioka2(nvk,k)=1
END IF
! LOWER AND UPPER BOND OF VARIABLE FOR CONDITION
READ(ch(nk+1:nk+i),*)oka(1,nvk,k),oka(2,nvk,k)
GO TO 112

!-- OKNYSK -------------------------------------------------------------

! NEW FOREST NR
120     READ(ch,*)k
ioknf1(1:mxvk1,k)=0; ioknf2(1:mxvk1,k)=0
nk=INDEX(ch,',')
IF(nk == 0)THEN
  WRITE(*,*)' OKNYSK - Wrong specification: ',trim(ch)
  GO TO 260
END IF
nvk=0
122     IF(INDEX(ch(nk+1:),'=') == 0)THEN
  ioknf1(nvk+1,k)=0
  GO TO 1
END IF
nk=nk+INDEX(ch(nk+1:),'=')
! NEW CONDITION
nvk=nvk+1
IF(nvk > mxvk)THEN
  WRITE(*,*)' OKNYSK - Too many conditions. Max is ',mxvk
  GO TO 260
END IF
! VARIABLE AND (MAYBE) SPECIES
IF(ch(nk-1:nk-1) == ')')THEN
  i1=INDEX(ch(1:nk),'(',.true.)+1
  ctemp=ch(i1:nk-2)
  READ(ctemp,*)ioknf3(nvk,k)
  i1=i1-3
ELSE
  ioknf3(nvk,k)=0
  i1=nk-2
END IF
CALL  g3jfr(122,cvar,2,ch(i1:i1+1),ioknf1(nvk,k))
IF(ioknf1(nvk,k) <= 0)THEN
  WRITE(*,*)' OKNYSK - Wrong def. of variabel: ',ch(nk-2:nk-1)
  GO TO 260
END IF
! CONNECTION TO NEXT CONDITION (* = AND; + = OR)
IF(INDEX(ch(nk+1:),'*')+INDEX(ch(nk+1:),'+') == 0)THEN
  i=nz-nk
  ioknf2(nvk,k)=0
ELSE IF(INDEX(ch(nk+1:),'*') == 0)THEN
  i=INDEX(ch(nk+1:),'+')-1
  ioknf2(nvk,k)=0
ELSE IF(INDEX(ch(nk+1:),'+') == 0)THEN
  i=INDEX(ch(nk+1:),'*')-1
  ioknf2(nvk,k)=1
ELSE IF(INDEX(ch(nk+1:),'+') < INDEX(ch(nk+1:),'*'))THEN
  i=INDEX(ch(nk+1:),'+')-1
  ioknf2(nvk,k)=0
ELSE
  i=INDEX(ch(nk+1:),'*')-1
  ioknf2(nvk,k)=1
END IF
! LOWER AND UPPER BOND OF VARIABLE FOR CONDITION
READ(ch(nk+1:nk+i),*)oknf(1,nvk,k),oknf(2,nvk,k)
GO TO 122

!-- AUTATG -------------------------------------------------------------

! TREATMENT NUMBER K
130     READ(ch,*)k
iauta1(1:mxvk1,k)=0; iauta2(1:mxvk1,k)=0
nk=INDEX(ch,',')
IF(nk == 0)THEN
  WRITE(*,*)' AUTATG - Wrong specification: ',trim(ch)
  GO TO 1
END IF
nvk=0
132     IF(INDEX(ch(nk+1:),'=') == 0)THEN
  iauta1(nvk+1,k)=0
  GO TO 260
END IF
nk=nk+INDEX(ch(nk+1:),'=')
! NEW CONDITION
nvk=nvk+1
IF(nvk > mxvk)THEN
  WRITE(*,*)' AUTATG - Too many conditions. Max is ',mxvk
  GO TO 260
END IF
! VARIABLE AND (MAYBE) SPECIES
IF(ch(nk-1:nk-1) == ')')THEN
  i1=INDEX(ch(1:nk),'(',.true.)+1
  ctemp=ch(i1:nk-2)
  READ(ctemp,*)iauta3(nvk,k)
  i1=i1-3
ELSE
  iauta3(nvk,k)=0
  i1=nk-2
END IF
CALL  g3jfr(122,cvar,2,ch(i1:i1+1),iauta1(nvk,k))
IF(iauta1(nvk,k) <= 0)THEN
  WRITE(*,*)' AUTATG - Wrong def. of variabel: ',ch(nk-2:nk-1)
  GO TO 260
END IF
! CONNECTION TO NEXT CONDITION (* = AND; + = OR)
IF(INDEX(ch(nk+1:),'*')+INDEX(ch(nk+1:),'+') == 0)THEN
  i=nz-nk
  iauta2(nvk,k)=0
ELSE IF(INDEX(ch(nk+1:),'*') == 0)THEN
  i=INDEX(ch(nk+1:),'+')-1
  iauta2(nvk,k)=0
ELSE IF(INDEX(ch(nk+1:),'+') == 0)THEN
  i=INDEX(ch(nk+1:),'*')-1
  iauta2(nvk,k)=1
ELSE IF(INDEX(ch(nk+1:),'+') < INDEX(ch(nk+1:),'*'))THEN
  i=INDEX(ch(nk+1:),'+')-1
  iauta2(nvk,k)=0
ELSE
  i=INDEX(ch(nk+1:),'*')-1
  iauta2(nvk,k)=1
END IF
! LOWER AND UPPER BOND OF VARIABLE FOR CONDITION
READ(ch(nk+1:nk+i),*)auta(1,nvk,k),auta(2,nvk,k)
GO TO 132

!-- EJUT1 --------------------------------------------------------------

140   noout1=1
GO TO 1

!-- UT1 ----------------------------------------------------------------

150   noout1=0
GO TO 1

!-- LOG ----------------------------------------------------------------

160   LOG=1
GO TO 1

!-- NOLOG --------------------------------------------------------------

170   LOG=0
GO TO 1

!-- GENMTD -------------------------------------------------------------

180   IF(INDEX(ch,',')==0)THEN
  ch(nz+1:nz+2)=',0'
  nz=nz+2
END IF
READ(ch,*)igen,id0
GO TO 1

!-- NPER ---------------------------------------------------------------

190   READ(ch,*)nper,tlength,tpush
forall(i=1:nper)year(i)=(i-1)*tlength + tpush
GO TO 1

!-- xxxxx --------------------------------------------------------------

200   READ(ch,*)tpush
GO TO 1

!-- PA1 ----------------------------------------------------------------

210   READ(ch,*)iper1
GO TO 1

!-- PAINT --------------------------------------------------------------

220   READ(ch,*)iper2
GO TO 1

!-- ASTOP---------------------------------------------------------------

230   READ(ch,*)ibreak
GO TO 1

!-- PAVAR --------------------------------------------------------------

240   READ(ch,*)ncp
GO TO 1

!-- RUN ----------------------------------------------------------------

250   CALL cpu_time(tb)
nrin =0
nrout=0
RETURN

!-- STOP ---------------------------------------------------------------

260   WRITE(*,*)CHAR(7)
STOP

!-- STAT ---------------------------------------------------------------

270   CALL cpu_time(tused)
i=(tused-ta)
j=(tused-tb)
WRITE(*,271)nritot,nrin,nrotot,nrout,i,j
271   FORMAT(//  &
    ' *********** GAYA - STATISTIK **************************'//  &
    '                          TOTALT         EFTER SISTA RUN'/  &
    '                       --------------    ---------------'/  &
    ' RECORDS READ          ',i14,i14/ ' RECORDS WRITTEN       ',i14,i14/  &
    ' CPU TIME (S)          ',i14,i14//  &
    ' *******************************************************'//)
GO TO 1

!-- DOMAIN -------------------------------------------------------------

280   IF(nz == 0)THEN
  indom =0
  ndom  =0
ELSE
  ndom=1               ! COUNT NO. OF COMMAS = NO. OF DOMAINS - 1
  DO  i=1,nz
    IF(ch(i:i) == ',')ndom=ndom+1
  END DO
  READ(ch,*)(indom(i),i=1,ndom)
END IF

GO TO 1

!-- YEARS --------------------------------------------------------------

290   nper=1                  ! COUNT NO. OF COMMAS = NO. OF PERIODS - 1
DO  i=1,nz
  IF(ch(i:i) == ',')nper=nper+1
END DO
READ(ch,*)(year(i),i=1,nper)
GO TO 1

!-- VERSION  -----------------------------------------------------------

300   READ(ch,'(a8)')version
GO TO 1

!-- SKIP  ---------------------------------------------------------------

310   iskip = 1
GO TO 1

!-- UNSKIP  -------------------------------------------------------------

320   iskip = 0
GO TO 1

END SUBROUTINE g3comd

SUBROUTINE g3jfr(ic1,comm,ic2,cc,nr)
!***********************************************************************
! USE: DETERMINES WHAT SUBSTRING IN COMM THAT CORRESPONDS WITH STRING
! IN CC. CC CAN BE SHORTENED COMPARED TO THE SUBSTRING IN COMM.
! CHARACTERS IN CC SHOULD BE LEFT ADJUSTED

!        IN: COMM = CHARACTER STRING/SUBSTR1 SUBSTR2..../
!            CC   = STRING THAT IS COMPARED TO SUBSTR1, SUBSTR2 ETC.

!       OUT: NR   = CC BLANK OR NO CORRESPONDENCE => 0
!                 = CC CORRESPONDS TO SEVERAL SUBSTRINGS => <0
!                 = CC CORRESPONDS TO ONE SUBSTRING => => NR=SUBSTR
!***********************************************************************
use g3_interfaces

INTEGER, INTENT(IN)                      :: ic1
CHARACTER (LEN=ic1), INTENT(IN)          :: comm
INTEGER, INTENT(IN)                      :: ic2
CHARACTER (LEN=ic2), INTENT(IN)          :: cc
INTEGER, INTENT(OUT)                     :: nr

nr=0
nz=0
k =0

!-- DETERMINE NO. OF NZ IN CC
DO  i=1,ic2
  IF(cc(i:i) /= ' ')nz=nz+1
END DO
IF(nz == 0)RETURN

!-- SEEK CORRESPONDENCE
l=ic1
i=1
20    k=k+1
DO  j=1,nz
  IF(comm(i+j-1:i+j-1) /= cc(j:j))GO TO 26
END DO
IF(nr /= 0)THEN
  nr=-nr
  RETURN
END IF
nr=k

26    DO  j=i,l
  IF(comm(j:j) == ' ')GO TO 30
END DO
RETURN
30    DO  i=j,l
  IF(comm(i:i) /= ' ')GO TO 20
END DO
RETURN

END SUBROUTINE g3jfr
