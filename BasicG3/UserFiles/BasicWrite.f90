      SUBROUTINE G3OUT(NR,M,IPER,IREC)
!***********************************************************************
!***********************************************************************
!-- GAYA-DECLARATIONS
	USE G3_Global
	USE G3_GAPER
	USE G3_GAATG
	USE G3_GAATGC
	USE G3_GAFRAM
    USE G3_NAMES
	USE G3_interfaces
	INTEGER	:: NR,M,IPER,IREC

!-- OWN DECLARATIONS
		INTEGER	:: istart,j,i,StandIDtmp
        REAL    :: Scost,Hcost,Decid
        CHARACTER   :: CTRT(7)*2
        DATA CTRT/'NM', 'IP', 'Cl', 'Th', 'Fe', 'FT', 'FF'/
        DATA istart/0/

!-- Start
	  	if(istart == 0 )then
      		write(NR,'(3a)')	&
     ' StandNo MgmProgram Period TreatmentCode,Treatment, Age Height Stems Diameter ', &
     'm3Vol m3Pine m3Spruce m3Deciduous',	&
     'm3Harvest CostSilviculture CostHarvest'
       		istart=1
            StandIDtmp = 0
     	 endif

    if(IFIX(StandID) /= StandIDtmp)then
      jNr = 0
      StandIDtmp=IFIX(StandID)
    endif
    jNr = jNr+1  

    do iiper=1,IPER 
       
      Decid = 0.
      do i=Birch,MXSPECI; Decid=Decid+ARTin(Vs,i,iiper); enddo
      Hcost = sum(CUT(UVs,:,iiper))*90.
     if(IXATG(nratg(iiper)) == FF )then
       Scost=12000.
     else
       Scost=0.0
     endif
		write(nr,'(i10,3i4,1x,a,i4,f6.1,i6,f6.1,5i6,2i10)') &
     IFIX(StandID), jNr, iiper,nratg(iiper),CTRT(IXATG(nratg(iiper))),	&
     nint(bestin(TotAge,iiper)),	&
     BESTin(Hdom,iiper),	&
     nint(BESTin(N,iiper)),	&
     BESTin(D,iiper),	&
     nint(bestin(V,iiper)),    &
     nint( ARTin(Vs,Pine,iiper) ),	&
     nint( ARTin(Vs,Spruce,iiper) ),	&
     nint( Decid) ,	&
     nint( sum(CUT(UVs,:,iiper) )),  &
     nint( Scost ), &
     nint( Hcost )
       

   enddo      

   	irec=1

      RETURN
      END
