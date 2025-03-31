    SUBROUTINE G3PREP   ! Obsolete. Calculation moved to G3GET
!***********************************************************************
! PROJECTS THE STATE TO YEAR(1), I.E. THE FIRST YEAR OF THE FIRST PERIOD
! BUILD ON THE SAME KIND OF PROJECTION LOGI! AS IN G3FRAM
!
!  PARAMETRAR:
!     ?    = (in) ?
!
!  CALLS: G3ETVX, G3RTVX, G3INPL
!***********************************************************************
	USE G3_Global
	USE G3_GAPER
	USE G3_GAFRAM
	USE G3_GAATG

    USE G3_NAMES
	USE G3_interfaces

	INTEGER ::    i,j,IPER,IFPER,ID
	REAL ::       x,eps
	DATA eps/0.1/

!--
      IPER=1
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

    RETURN  
    END

      SUBROUTINE G3FRAM(ICONT,NDP,IATG,IPER,IERR)
!***********************************************************************
!***********************************************************************
    USE G3_Global
    USE G3_GAPER
    USE G3_GAATG
    USE G3_GAFRAM
    USE G3_NAMES
	USE G3_interfaces
    INTEGER, intent(in) ::  ICONT,NDP,IATG(MXPER)
    INTEGER, intent(out) ::  IPER,IERR
!--
    REAL    ::  eps
    DATA eps/0.1/
!--
    IERR=0

!-- PROJECTION ------------------------------------------------------
    DO 100 IPER=ICONT,NPER
        call G3ATB(IPER,NM,ARTin(:,:,IPER),BESTin(:,IPER),IFIX,FIX)
        IFPER=IPER+1
!-- EXECUTE TREATMENT - TEST IF RETURN ------------------------------      
        ID=IATG(IPER)
        CALL  G3ATG(IPER,ID,IERR)
!        write(*,*) iper, ID, ierr
		IF(IERR == 1)RETURN
        BESTout(:,IPER)=BESTin(:,IPER)
        call G3ATB(IPER,IXATG(ID),ARTout(:,:,IPER),BESTout(:,IPER),IFIX,FIX)
		IF(ID.EQ.IBREAK .OR. IPER.EQ.NPER)RETURN
!-- GROWTH PROJECTION INCLUDING VOLUME CALCULATION --------------------
       CALL  G3TVX(IPER)

        BESTin(:,IFPER)=BESTout(:,IPER)
        BESTin(TotAge,IFPER)=BESTout(TotAge,IPER)+Year(IFPER)-Year(IPER)
        BESTin(TimeTh,IFPER)=BESTout(TimeTh,IPER)+Year(IFPER)-Year(IPER)
        BESTin(TimeCl,IFPER)=BESTout(TimeCl,IPER)+Year(IFPER)-Year(IPER)
        BESTin(TimeFe,IFPER)=BESTout(TimeFe,IPER)+Year(IFPER)-Year(IPER)
        
100   CONTINUE

      END

      SUBROUTINE G3ATG(IPER,ID,IERR)
!***********************************************************************
!***********************************************************************
        USE G3_Global
        USE G3_GAATG
        USE G3_GAATGC
        USE G3_GAFRAM
        USE G3_NAMES
    	USE G3_interfaces
        INTEGER ::  IPER,ID,IERR

!-- TEST THAT TREATMENT IS ALLOWED
      IF(IG3CHK(IOKA1(1,ID),IOKA2(1,ID),IOKA3(1,ID),OKA(1,1,ID),IPER).EQ.0)THEN
        IERR=1
        RETURN
      ENDIF
      NRATG(IPER)=ID

!-- EXECUTE TREATMENT

      CALL  G3IN2OUT(ID,IPER,IERR)  

      RETURN
      END

    SUBROUTINE G3IN2OUT(ID,IPER,IERR)
!***********************************************************************
! Come here to update ARTin to ARTout as a function of treatment
!***********************************************************************
    USE G3_Global
    USE G3_GAATG
    USE G3_GAFRAM
    USE G3_GANEWF
    USE G3_NAMES
    USE G3_interfaces
    INTEGER ::  ID,IPER,IERR
!
    INTEGER :: i,NewFnr,j
    REAL    :: ARTleft(NART),ATG0(NTREAT)
    REAL    :: Nx,Gx,RD
    LOGICAL :: NoCUT(MXSPECI)

!--
!-- ZERO
	IERR=0
	CUT(:,:,IPER)=0.0
	ARTout(:,:,IPER)=0.

!-- FERTILIZATION OR DO NOTHING
    IF(IXATG(ID).EQ.NM.OR.IXATG(ID).EQ.Fe)THEN
 		ARTout(:,:,IPER)= ARTin(:,:,IPER)
       	RETURN

!-- INPLANTING --------------------------------------------------------
    ELSEIF(IXATG(ID).EQ.IP)THEN
        NewFnr=NINT(ATG(NFnr,0,ID))
        ARTout(:,:,IPER)=FNART(1:NART,1:MXSPECI,NewFnr)
    
!-- FINAL FELLING -----------------------------------------------------
    ELSEIF(IXATG(ID).EQ.FF)THEN
        forall(i=1:MXSPECI)CUT(UVs,I,IPER)=ARTin(Vs,I,IPER)
        forall(i=1:MXSPECI)CUT(UNs,I,IPER)=ARTin(Ns,I,IPER)
        do i=1,MXSPECI; CUT(UDs,I,IPER)=G3DIA( ARTin(Gs,I,IPER),ARTin(Ns,I,IPER) ); enddo
        forall(i=1:MXSPECI)CUT(UHs,I,IPER)=ARTin(Hs,I,IPER)
        ARTout(:,:,iper)=0.
 
!-- CLEANING/THINNING -------------------------------------------------
    ELSEIF(IXATG(ID).EQ.Cl.OR.IXATG(ID).EQ.Th.OR.IXATG(ID).EQ.FT)THEN
		NoCUT(:)=.true.
        ARTleft(:)=0.
		DO I=1,MXSPECI
			IF(ARTin(NS,I,IPER) > 10.)THEN	! Bara om träd finns
	! FINNS ÅTGÄRDEN SEPARAT DF FÖR TRÄDSLAGET
				DO J=1,MXSPECI
					IF(ISPEC_ATG(J,ID).EQ.I)then
	! Transfer to treatment array ATG0
						NoCUT(i)=.false.
						call Thin(ATG(:,i,ID),ARTin(:,i,IPER), Nx,Gx,RD,IERR)
						if(IERR ==1)then
							Nx=0.;Gx=0.
						endif
						ARTout(Ns,i,IPER)=ARTin(Ns,i,IPER)*(1.-Nx)
						ARTout(Gs,i,IPER)=ARTin(Gs,i,IPER)*(1.-Gx)
						ARTout(Vs,i,IPER)=ARTin(Vs,i,IPER)*(1.-Gx)
					endif
				enddo
			endif
		enddo
		ARTleft(Ns)=sum(ARTin(Ns,:,IPER),NoCUT(:)) + sum(ARTout(Ns,:,IPER))
		ARTleft(Gs)=sum(ARTin(Gs,:,IPER),NoCUT(:)) + sum(ARTout(Gs,:,IPER))
		ARTleft(Vs)=sum(ARTin(Vs,:,IPER),NoCUT(:)) + sum(ARTout(Vs,:,IPER))
		ATG0(:)=ATG(:,0,ID)
        if(ATG0(StrTh) > 0.)then
        	ATG0(StrTh)=ATG0(StrTh)-(sum(ARTin(Gs,:,IPER))- ARTleft(Gs))/sum(ARTin(Gs,:,IPER))
          	if(ATG0(StrTh) <= 0.)then
         		IERR = 1
          		RETURN
            endif
        endif
		call Thin(ATG0,ARTleft, Nx,Gx,RD,IERR)
        if(ATG(StrTh,0,ID)*ATG(UDrel,0,ID) == 0.)then
        	Nx=Nx*ARTleft(Ns)/(sum(ARTin(Ns,:,IPER),NoCUT(:))+ 0.0001)
        	Gx=Gx*ARTleft(Gs)/(sum(ARTin(Gs,:,IPER),NoCUT(:))+ 0.0001)
            if(Nx > 1. .or. Gx > 1.) IERR=1
        endif
		if(IERR ==1)RETURN
		DO I=1,MXSPECI
			if(NoCUT(i))then
				ARTout(Ns,i,IPER)=ARTin(Ns,i,IPER)*(1.-Nx)
				ARTout(Gs,i,IPER)=ARTin(Gs,i,IPER)*(1.-Gx)
				ARTout(Vs,i,IPER)=ARTin(Vs,i,IPER)*(1.-Gx)
			endif
		enddo
		DO I=1,MXSPECI
			CUT(UVs,I,IPER)=ARTin(Vs,I,IPER)-ARTout(Vs,I,IPER)
			CUT(UNs,I,IPER)=ARTin(Ns,I,IPER)-ARTout(Ns,I,IPER)
			CUT(UDs,I,IPER)=G3DIA(ARTin(Gs,i,IPER)-ARTout(Gs,i,IPER),CUT(UNs,I,IPER))
			CUT(UHs,I,IPER)=ARTin(Hs,I,IPER)
			!+(ARTin(Hs,:,IPER)-ARTOut(Hs,:,IPER))/ &
			!(G3DIA(ARTin(Gs),ARTin(Ns))-G3DIA(ARTin(Gs),ARTin(Ns))+0.001)*(D2-D1)
		enddo
        ! Make final adjustments
		DO I=1,MXSPECI
        	ARTout(Hs,i,IPER)=ARTin(Hs,i,IPER)
        	ARTout(BHAs,i,IPER)=ARTin(BHAs,i,IPER)
        	ARTout(Ms,i,IPER)=ARTin(Ms,i,IPER)
        	if(ARTout(Ns,i,IPER) < 1.)then	! Remove species with less than 1 stem after harvest
            	ARTout(:,i,IPER)=0.
            endif
        enddo
	 
	ENDIF

    RETURN
    END

      FUNCTION IG3CHK(IXY1,IXY2,IXY3,XY,IPER) result(IGxCHK)
!***********************************************************************
!***********************************************************************
	USE G3_Global
	USE G3_GAFRAM
	USE G3_interfaces
	INTEGER :: IXY1(MXVK1),IXY2(MXVK1),IXY3(MXVK1),IPER,IGxCHK
	REAL	:: XY(2,MXVK)

	INTEGER		V2A(22)

! CONNECTION TO ART
	DATA		V2A/ 5,99,99,99, 1, 3, 6,99, 4,99, &
     			    99,99,99,99,99,99,99,99,99,99, &
     			     7,99/
!				    01,02,03,04,05,06,07,08,09,10,

! BEST: 1-20; IFIX: 21-32; FIX: 33-40; PR:41
      REAL*4 VAR(41)

!-- BRING OVER VALUES TO VAR ------------------------------------------

      DO I=1,20; VAR(I)=BESTin(I,IPER); ENDDO
      DO I=21,32; VAR(I)=IFIX(I-20); ENDDO
      DO I=33,39; VAR(I)=FIX(I-32); ENDDO
      VAR(40)=IPER

!-- CONTROL ON VAR AND ART --------------------------------------------

      IGxCHK=1
      IF(IXY1(1).EQ.0)RETURN
      I=0

20    I=I+1
	IF(IXY3(I).EQ.0)THEN
        IF(VAR(IXY1(I)).LT.XY(1,I).OR.VAR(IXY1(I)).GT.XY(2,I))IGxCHK=0
	ELSE
		K=0
		DO J=1,MXSPECI; IF(IXY3(I).EQ.J)K=J; ENDDO
		IF(K.EQ.0)THEN
			IGxCHK=0
		ELSE
            IF(ARTin(V2A(IXY1(I)),K,IPER).LT.XY(1,I).OR.    &
     		   ARTin(V2A(IXY1(I)),K,IPER).GT.XY(2,I))IGxCHK=0
		ENDIF
	ENDIF
      IF(IXY2(I).EQ.0)THEN
		IF(IGxCHK.EQ.1)RETURN
		IF(IXY1(I+1).EQ.0)RETURN
		IGxCHK=1
      ENDIF
      GO TO 20

      END


      FUNCTION G3DIA(GRYTA,STAM) result(GxDia)
!***********************************************************************
!***********************************************************************
	USE G3_interfaces
	REAL	:: GRYTA,STAM,GxDia
      IF(STAM.LT.0.01)THEN
         GxDIA=0.0
      ELSE
         GxDIA=SQRT(GRYTA*12732.4/STAM)
      ENDIF
      RETURN
      END

      FUNCTION G3YTA(DIA,STAM) result(GxYTA)
!***********************************************************************
!***********************************************************************
	USE G3_interfaces
	REAL	:: DIA,STAM,GxYTA
      GxYTA=DIA*DIA*STAM/12732.4
      RETURN
      END

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
	USE G3_interfaces

    INTEGER	:: IERR
	REAL    :: ATG(NTREAT),ARTin(NART),Nx,Gx,RD

! Åtgärder o kombinationer i prioritetsordning
!	 GS GE SU SE RD
!	  1  2  3  4  5
!	1 x     x
!	2 x        x
!	3 x           x
!	4    x  x
!	5    x     x
!	6    x        x
!	7       x     x  
!	8          x  x
!   BERKNA UTTAGS-% P GRUNDYTA (Gx), STAMANTAL (Nx) OCH RELDIA (RD)

	Gx=0.; Nx=0.; RD=0.
	if(ATG(StrTh) == 1)then						! "kalavverkning" enskilt trädslag
		Gx=1
		Nx=1.
		RD=1.
	ELSEIF(ATG(StrTh)*ATG(UN).NE.0.0)THEN
		Gx=ATG(StrTh)-Gx
		Nx=ATG(UN)/ARTin(Ns)
		RD=SQRT(Gx/Nx)
	ELSEIF(ATG(StrTh)*ATG(NaTh).NE.0.0)THEN
		Gx=ATG(StrTh)-Gx
		Nx=(ARTin(Ns)-ATG(NaTh))/ARTin(Ns)
		IF(Nx.GT.0.0)RD=SQRT(Gx/Nx)
	ELSEIF(ATG(StrTh)*ATG(UDrel).NE.0.0)THEN
		Gx=ATG(StrTh)-Gx
		RD=ATG(UDrel)
		Nx=Gx/(RD*RD)
	ELSEIF(ATG(GaTh)*ATG(UN).NE.0.0)THEN
		Gx=(ARTin(Gs)-ATG(GaTh))/ARTin(Gs)
		Nx=ATG(UN)/ARTin(Ns)
		IF(Gx.GT.0.0)RD=SQRT(Gx/Nx)
	ELSEIF(ATG(GaTh)*ATG(NaTh).NE.0.0)THEN
		Gx=(ARTin(Gs)-ATG(GaTh))/ARTin(Gs)
		Nx=(ARTin(Ns)-ATG(NaTh))/ARTin(Ns)
		IF(Gx.GT.0.0.AND.Nx.GT.0.0)RD=SQRT(Gx/Nx)
	ELSEIF(ATG(GaTh)*ATG(UDrel).NE.0.0)THEN
		Gx=(ARTin(Gs)-ATG(GaTh))/ARTin(Gs)
		RD=ATG(UDrel)
		Nx=Gx/(RD*RD)
	ELSEIF(ATG(UN)*ATG(UDrel).NE.0.0)THEN
		Nx=ATG(UN)/ARTin(Ns)
		RD=ATG(UDrel)
		Gx=Nx*(RD*RD)
	ELSEIF(ATG(NaTh)*ATG(UDrel).NE.0.0)THEN
		Nx=(ARTin(Ns)-ATG(NaTh))/ARTin(Ns)
		RD=ATG(UDrel)
		Gx=Nx*(RD*RD)
	ENDIF

    IERR=0
	IF(MIN(Gx,Nx,RD).LE.0.0.OR.MAX(Gx,Nx,RD-1.).GT.1.)IERR=1

	RETURN
    END
    