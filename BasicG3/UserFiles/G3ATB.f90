     SUBROUTINE G3ATB(IPER,IXATG,ART,BEST,IFIX,FIX)
!***********************************************************************
!  Come here to compute:
!   - non-dynamic ART variables
!   - BEST treatment related variables
!   - BEST variables as a function of ART
!***********************************************************************
	USE G3_Global
    USE G3_NAMES
	USE G3_interfaces
    INTEGER :: IPER,IXATG,IFIX(NFIX)
    REAL	:: ART(NART,MXSPECI),BEST(NBEST),FIX(NFIX)
!-- Local
    INTEGER :: i,DomSpec

!-- Update non-dynamic ART variables

    do i=1,MXSPECI
      ART(Vs,i)=G3VOL(I,ART(1,i),IFIX,FIX)
    enddo

!-- Treatment related items
      if(IXATG == FF)then
        BEST(TotAge)=0.
        BEST(TimeTh)=0.
        BEST(TimeCl)=0.
        BEST(TimeFe)=0.
        BEST(PerFF) = IPER
      endif

    if( IXATG.EQ.Cl)BEST(TimeCl)=0.
    if( IXATG .EQ.Th .OR. IXATG.EQ.FT)BEST(TimeTh)=0.
    if( IXATG .EQ.FT)BEST(TimeFe)=0.

!-- Update BEST based on ART
! 5, 6, 7 and 13
      BEST(N)=SUM(ART(Ns,1:MXSPECI))
      BEST(G)=SUM(ART(Gs,1:MXSPECI))
      BEST(V)=SUM(ART(Vs,1:MXSPECI))
      BEST(Mort)=SUM(ART(Ms,1:MXSPECI))

! 8
      BEST(D)=G3DIA(BEST(G),BEST(N))

 ! 9
    DomSpec = MAXLOC(ART(Ns,1:MXSPECI),1)
    if(BEST(G) > 0.)DomSpec = MAXLOC(ART(Gs,:),1)
    BEST(Hdom)= ART(Hs,DomSpec)
    
! 14, 15 OCH 16
! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch
      BEST(SpPin)=( ART(Vs,Pine) + ART(Vs,Contorta) )/(BEST(V)+0.001)
      BEST(SpSpr)=ART(Vs,Spruce)/(BEST(V)+0.001)
      BEST(SpDec)=1.-BEST(SpPin)-BEST(SpSpr)
      if(BEST(SpDec) < 0.001)BEST(SpDec)=0.0

      RETURN
      END
