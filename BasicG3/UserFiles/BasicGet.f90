      SUBROUTINE G3GET(NR,LREC,OLDFOR,M,NDOM,INDOM,IERR,IEND)
!***********************************************************************
!***********************************************************************
!-- GAYA-DEKLARATIONER
	USE G3_Global
	USE G3_GAFRAM
	USE G3_GANEWF
    USE G3_NAMES
    USE G3_GAATG
	USE G3_interfaces
	LOGICAL :: OLDFOR
	INTEGER :: NR,LREC,M,INDOM(NDOM),IERR,IEND

!-- own DECLARATIONS
	  INTEGER 	:: i,NoOfSpecies
     DATA NoOfSpecies/3/

      IERR=0
      IEND=0
      IPER=1
      
!-- ZERO ------------------------------------------------------

	IFIX	= 0
	FIX		= 0.
	BESTin(:,:)	= 0.
	ARTin(:,:,:)	= 0.

!-- READ IN VALUES ------------------------------------------------

      IF(OLDFOR .and. LREC.EQ.0)THEN
        READ(NR,*,END=99) &
     	IFIX(StandID), &	! Beståndsnummer
     	IFIX(Domain), &		! Class
        IFIX(Sapling), &	! Sapling 1, otherwise 0
        (ARTin(Ns,i,IPER), i=1,NoOfSpecies), & ! Stems >= 5 cm
     	(ARTin(Gs,i,IPER), i=1,NoOfSpecies), & ! Basal area (m2)
     	(ARTin(Hs,i,IPER), i=1,NoOfSpecies), & ! Height (m)
     	(ARTin(BHAs,i,IPER), i=1,NoOfSpecies), & ! Age at brh (total age when read, modified below)
     	(ARTin(Vs,i,IPER), i=1,NoOfSpecies)  ! Volume (m3sk/ha)
        IF( IFIX(Sapling) == 1 .and. sum(ARTin(Ns,:,IPER)) < 100. ) then
          ARTin(Ns,:,IPER) =0.0 ! Make barren land of saplings with few trees
          ARTin(Gs,:,IPER) =0.0
          ARTin(Hs,:,IPER) =0.0
          ARTin(Vs,:,IPER) =0.0
        ELSEIF( IFIX(Sapling) == 1 ) then
          forall(i=1:MXSPECI) ARTin(Vs,i,IPER) = ARTin(Gs,i,IPER)*4.
          ARTin(Gs,:,IPER) =0.0 ! Remove basal area of saplings
        ENDIF
        call G3ATB(IPER,NM,ARTin(:,:,IPER),BESTin(1,IPER),IFIX,FIX)

! New forest
      elseif(.not.OLDFOR)then
      	 READ(NR,*,END=99) &
! Stems ha-1         
         FNART(Ns,Pine,M),FNART(Ns,Spruce,M),FNART(Ns,Birch,M), &
! Basal area (m2)
         FNART(Gs,Pine,M),FNART(Gs,Spruce,M),FNART(Gs,Birch,M), &
! Height (m)
         FNART(Hs,Pine,M),FNART(Hs,Spruce,M),FNART(Hs,Birch,M), &
! Age 
         FNART(BHAs,Pine,M),FNART(BHAs,Spruce,M),FNART(BHAs,Birch,M)
        RETURN
    endif
        if(sum(ARTin(Gs,:,IPER)) == 0.)then
          BESTin(TotAge,IPER)=0.
          do i=1,MXSPECI; BESTin(TotAge,IPER)= &
            BESTin(TotAge,IPER)+ARTin(BHAs,i,IPER)*ARTin(Ns,i,IPER); enddo
          BESTin(TotAge,IPER)=BESTin(TotAge,IPER)/(sum(ARTin(Ns,:,IPER))+0.001)
        else  
          BESTin(TotAge,IPER)=0.
          do i=1,MXSPECI; BESTin(TotAge,IPER)= &
            BESTin(TotAge,IPER)+ARTin(BHAs,i,IPER)*ARTin(Gs,i,IPER); enddo
          BESTin(TotAge,IPER)=BESTin(TotAge,IPER)/(sum(ARTin(Gs,:,IPER))+0.001)
        endif
        BESTin(TimeTh,IPER)=BESTin(TotAge,IPER) 
        BESTin(TimeCl,IPER)=BESTin(TotAge,IPER)
        BESTin(TimeFe,IPER)=BESTin(TotAge,IPER)

          
!-- SKIP IF NOT IN DOMAIN
	if(OLDFOR .and. ndom.ne.0)then
		do 22 i=1,ndom
22			if(IFIX(Domain).eq.indom(i))go to 24
		ierr=1
		RETURN
	endif

24		write(*,*)M,IFIX(1)
	RETURN

! EOF
99    IEND=1

      RETURN
      END
