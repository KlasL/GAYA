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
    save
	INTEGER	:: NR,M,IPER,IREC

!-- OWN DECLARATIONS
		INTEGER	:: istart,j,i
        REAL    :: Revenue,CostSilv,CostHarv,Timber(MXSPECI),Pulpwood(MXSPECI),Residues(MXSPECI)
        CHARACTER   :: CTRT(7)*2,StandIDtmp*80
        DATA CTRT/'NM', 'IP', 'Cl', 'Th', 'Fe', 'FT', 'FF'/
        DATA istart/0/

!-- Start
	  	if(istart == 0 )then
      		write(NR,'(3a)')	&
    'StandNo MgmProgram Period Treatment Age Height Stems Diameter Volume ', &
    'm3Harvest Costs Revenues ',    &
    'VolSpec1 VolSpec2 VolSpec3 VolSpec4 VolSpec5 VolOtherSpec'
       		istart=1
            StandIDtmp = ' '
     	 endif

    if(CFIX(StandID) /= StandIDtmp)then
        jNr = 0
        StandIDtmp=CFIX(StandID)
    endif

    jNr = jNr+1  
    do iiper=1,IPER 
       
     call Resoursces(IXATG(nratg(iiper)),BESTin(:,iiper),Cut(:,:,iiper), &
         Revenue,CostSilv,CostHarv,Timber,Pulpwood,Residues,IERR)
        
     write(nr,'(a,i6,i3,1x,i3,2(i6,f6.1),99i6)') &
         TRIM(CFIX(StandID)), &
         jNr, &
         iiper, &
         nratg(iiper),	&
         nint(bestin(TotAge,iiper)),	&
         BESTin(Hdom,iiper),	&
         nint(BESTin(N,iiper)),	&
         BESTin(D,iiper),	&
         nint(bestin(V,iiper)),    &
         nint( sum(CUT(UVs,:,iiper))),  &
         nint( CostSilv +  CostHarv ), &
         nint( Revenue ), &
         nint( ARTin(Vs,1,iiper) ),	&
         nint( ARTin(Vs,2,iiper) ),	&
         nint( ARTin(Vs,3,iiper) ),	&
         nint( ARTin(Vs,4,iiper) ),	&
         nint( ARTin(Vs,5,iiper) ),	&
         nint( sum(ARTin(Vs,6:MXSPECI,iiper)) )

    enddo      

    irec=1

      RETURN
      END
