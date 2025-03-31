      SUBROUTINE G3TVX(IPER)
!***********************************************************************
!***********************************************************************
!-- GAYA-DEFINITIONER
	USE G3_Global
    USE G3_GAFRAM
	USE G3_NAMES
    USE G3_interfaces
    INTEGER ::  IPER
!-- LOCAL
    INTEGER ::  IFPER
    
    IFPER = IPER+1
    if(BESTout(G,IPER) > 0.1)then
      call ETVX(IPER)
    elseif(BESTout(N,IPER) > 10.)then
      call RTVX(IPER)
    else
      BESTin(TotAge,IFPER)=BESTout(TotAge,IPER)+5.
      BESTin(TimeTh,IFPER)=BESTout(TimeTh,IPER)+5.
      BESTin(TimeCl,IFPER)=BESTout(TimeCl,IPER)+5.
      BESTin(TimeFe,IFPER)=BESTout(TimeFe,IPER)+5.
      ARTin(:,:,IFPER)=0.0
    endif

    RETURN
    END
     
      SUBROUTINE ETVX(IPER)
!***********************************************************************
!***********************************************************************
!-- GAYA-DEFINITIONER
        USE G3_Global
        USE G3_GAATG
        USE G3_GAFRAM
        USE G3_interfaces
        USE G3_NAMES
 !-----------------------------------------------------------------------
    INTEGER ::  IPER

!-- LOCAL
		INTEGER	:: i,IFPER
		REAL	:: GI(MXSPECI),SG(MXSPECI),SN(MXSPECI),IG(MXSPECI),IN(MXSPECI),HI(MXSPECI)

        IFPER=IPER+1
!-- GROWTH etc. --------------------------------------------------------

! Growth basal area
	forall(i=1:MXSPECI)GI(i)=ARTout(Gs,i,IPER)*0.03*5.
! Growth height
	forall(i=1:MXSPECI)HI(i)=0.4*5.
! Mortality
	forall(i=1:MXSPECI) SG(I)=ARTout(Gs,i,IPER)*0.005*5.
	forall(i=1:MXSPECI) SN(I)=ARTout(Ns,i,IPER)*0.005*5.
! Ingrowth
	do i=1,MXSPECI; IN(i)=60.*ARTout(Gs,i,IPER)/BESTout(G,IPER); enddo
	do i=1,MXSPECI;IG(i)=G3YTA(5.,IN(i));enddo

!-- Update state -------------------------------------------------------

     forall(i=1:MXSPECI)ARTin(Gs,I,IFPER)=ARTout(Gs,I,IPER)+GI(I)-SG(I)+IG(I)
     forall(i=1:MXSPECI)ARTin(Ns,I,IFPER)=ARTout(Ns,I,IPER)-SN(I)+IN(I)
     forall(i=1:MXSPECI)ARTin(Hs,I,IFPER)=ARTout(Hs,I,IPER)+HI(I)
     forall(i=1:MXSPECI)ARTin(BHAs,I,IFPER)=ARTout(BHAs,I,IPER)+5.
     forall(i=1:MXSPECI)ARTin(Vs,I,IFPER)=ARTin(Gs,I,IFPER)*ARTin(Vs,I,IPER)/(ARTin(Gs,I,IPER)+0.001)

     BESTin(TotAge,IFPER)=BESTout(TotAge,IPER)+5.
     BESTin(TimeTh,IFPER)=BESTout(TimeTh,IPER)+5.
     BESTin(TimeCl,IFPER)=BESTout(TimeCl,IPER)+5.
     BESTin(TimeFe,IFPER)=BESTout(TimeFe,IPER)+5.
 
     RETURN
     END


      SUBROUTINE RTVX(IPER)
!***********************************************************************
!***********************************************************************
!-- GAYA-DECLARATIONS
        USE G3_Global
        USE G3_GAATG
        USE G3_GAFRAM
        USE G3_interfaces
        USE G3_NAMES
 !-----------------------------------------------------------------------

!-- LOCAL DECLARATIONS
		INTEGER	:: i,IFPER

        IFPER=IPER+1

!-- Update age
     BESTin(TotAge,IFPER)=BESTout(TotAge,IPER)+5
     BESTin(TimeTh,IFPER)=BESTout(TimeTh,IPER)+5.
     BESTin(TimeCl,IFPER)=BESTout(TimeCl,IPER)+5.
     BESTin(TimeFe,IFPER)=BESTout(TimeFe,IPER)+5.
      ARTin(:,:,ifper)=ARTout(:,:,iper)
         do i=1,MXSPECI
            if(ARTout(Ns,i,IPER)> 10.)ARTin(BHAs,i,IFPER)=ARTout(BHAs,i,IPER)+5.
        enddo

!-- Turn to established forest if old enough
    if(BESTin(TotAge,IFPER) > 20.) then
        do i=1,MXSPECI; ARTin(Gs,I,IFPER)=G3YTA(8.,ARTin(Ns,I,IFPER)); enddo
        forall(i=1:MXSPECI,ARTin(Gs,I,IFPER)>0.)ARTin(Hs,I,IFPER)=10.
        forall(i=1:MXSPECI)ARTin(Vs,I,IFPER)= &
            ARTin(Gs,I,IFPER)*6.
    endif

      RETURN
      END

	REAL FUNCTION G3VOL(I,ART,IFIX,FIX) result(GxVOL)
!***********************************************************************
!  Static volume assessment for species no. I
!***********************************************************************
 	USE G3_Global
    USE G3_NAMES
	USE G3_interfaces
    INTEGER :: I,IFIX(NFIX)
    REAL    :: FIX(NFIX),ART(NART)
!      
    real ::     fh
    DATA		fh/4./	! Form height
	INTEGER	:: its

	GxVOL=art(Gs) * fh

	return
    end
 

	REAL FUNCTION G3HOJD(I,ART,IFIX,FIX) result(GxHOJD)
!***********************************************************************
!  Static volume assessment for species no. I
!***********************************************************************
 	USE G3_Global
    USE G3_NAMES
	USE G3_interfaces
    INTEGER :: IFIX(NFIX)
    REAL    :: FIX(NFIX),ART(NART)
    REAL    :: Ds
!      
    real ::     fh
    DATA		fh/4./	! Form height
	INTEGER	:: i,its

    Ds = G3DIA(art(Gs),art(Ns))
    GxHOJD=fh*art(Gs)*3.*4.*10000/(3.14*Ds*Ds*art(Ns)+0.001) * 2.

	return
    end
