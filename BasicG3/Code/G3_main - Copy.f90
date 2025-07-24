PROGRAM gaya3

! Code converted using TO_F90 by Alan Miller
! Date: 2024-02-09  Time: 18:36:21

!***********************************************************************
! MAIN ROUTINE IN GAYA - ADMINISTRATION OF COMPUTATIONS OF TREATMENT
! PROGRAMS

! THE NAME OF THE COMMAND FILE IS GIVEN AS PARAMETER WHEN CALLING GAYA

!  ROUTINES CALLED: G3COMD, G3GET, G3FRAM, G3OUT
!***********************************************************************

!-- COMMON DEFINITIONS
use g3_global
use g3_gamain
use g3_gaper
use g3_gaatg
use g3_gafram
use g3_interfaces

!-- LOCAL DEFINITIONS

INTEGER*4 iatg(mxper),minnratg,m
CHARACTER (LEN=124) :: fname
DATA m/0/

CALL  g3init

!-- TEST FILE ---------------------------------------------------------
!      call TEST        ! Call if test needed in context

!-- OPEN COMMAND FILE -------------------------------------------------
WRITE(*,'(a\)')'fname= '
READ(*,'(a)')fname
OPEN(nrfil(8),FILE=fname,STATUS='old',ERR=5)
GO TO 10
5     WRITE(*,*)' *** ERROR OPENING COMMAND FILE ***'
STOP

!-- KOMMANDOHANTERING --------------------------------------------------
10      CALL  g3comd

!-----------------------------------------------------------------------
!--------------------- RUN ---------------------------------------------
!-----------------------------------------------------------------------

!-- READ RECORD FROM FOREST FILE ---------------------------------------

101   m=m+1
CALL  g3get(nrfil(3),lrsk,.true.,m,ndom,indom,ierr,iend)
IF(iend == 1)GO TO 10
IF(ierr == 1)GO TO 101
nrin=nrin+1

!-- GENERATE ALTERNATIVES WITH METHOD 3: COMPLETE ENUMERATION ----------------
!-- GENERATE ALTERNATIVES WITH METHOD 1: ACCORDING TO IATAG(:) FROM G3GET ----

ncpm=nper-MOD(nper-iper1,iper2)
IF(ncpm < iper1)GO TO 10
DO i=natg,1,-1
    IF(ixatg(i) /= 0)minnratg=i
END DO
DO  i=iper1,ncpm,iper2
  iatg(i)=minnratg
END DO
icont=1

130   CALL  g3fram(icont,ncpm,iatg,iper,ierr)

IF(ierr == 0)THEN
  CALL  g3out(nrfil(1),m,iper,irec)
  nrout=nrout+irec
END IF

icont=iper
icont=icont-MOD(icont-iper1,iper2)
135 iatg(icont)=iatg(icont)+1
IF(iatg(icont)<= natg .AND. ixatg(iatg(icont)) /= 0)GO TO 130
IF(iatg(icont)< natg)GO TO 135
iatg(icont)=minnratg
icont=icont-iper2
IF(icont >= iper1)GO TO 135

GO TO 101
END PROGRAM gaya3
