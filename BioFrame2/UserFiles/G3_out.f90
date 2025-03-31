      SUBROUTINE G3OUT(NR,M,IPER,IREC)
!***********************************************************************
!  Rutin f”r utskrift av alternativ. Kallas efter  om IERR i denna
!  rutin lika med 0.
!
!  PARAMETRAR:
!     NR    = (in) logiskt nummer (”ppnas mot fil med kommandot FOUT)
!     M     = (in) ordningsnummer p† best†nd/yta f”r vilket ber„kning
!                  skett (svarar mot parameter M i rutin G3GET)
!     IPER  = (in) antal perioder f”r vilka ber„kningar skett
!     IREC  = (ut) antal records som skrivs ut i rutinen (normalt=1)
!***********************************************************************
!-- GAYA-DEKLARATIONER
	USE G3_Global
    USE G3_GAMAIN
	USE G3_GAPER
	USE G3_GAATG
    USE G3_GAATGC
	USE G3_GAFRAM
	USE G3_NAMES 
	USE G3_interfaces
    save
	INTEGER	:: NR,M,IPER,IREC

!-- EGNA DEKLARATIONER
		INTEGER	:: irecSUM,istart,Pnr,NoOfThin,j,i,k,t,s
        INTEGER	:: icp,InplantPeriods,Rstart(2),Rstop(2)
        CHARACTER :: Ver*2,jNr*7,FactorFile*80,Bnr*16,CstandId*16
      	real     rtemp(mxper),ctemp(MXSPECI,mxper),AmenityValue
        DATA 	istart,irecSUM,Pnr/3*0/,Bnr/' '/
        REAL    :: CostSilv(MXPER),CostHarv(MXPER), &
            restim(MXSPECI,MXPER),resmav(MXSPECI,MXPER),resreject(MXPER)
        REAL	:: hd
		DATA        hd/10./                ! dominant height for switch to establised forest
        CHARACTER   :: CTRT(7)*4
        DATA CTRT/'"NM"', '"IP"', '"Cl"', '"Th"', '"Fe"', '"FT"', '"FF"'/
       logical	:: WriteTest,maxThinnings,Fertilization,Lodgepole,CCF,FirstWrite,ModelII
       integer  :: CurrAct(MXPER),FFn(MXPER),F0,F1
       DATA	WriteTest/.false./,ModelII/.true./

! Biomass
        REAL :: TSbiom(mxper), TScoar(mxper), TSfine(mxper), ResStm(mxper), ResRot(mxper)
        REAL :: ATo,ACo,BCo,AFi,AGr,Ast,BFi,ACoD,BCoD,AFiD,BFiD,FineRoots,BelowGrondBiomass
        REAL :: TTo,TTm,TTi,TTb,ABa,m3fub
		REAL ::	dia0,dia1,mmin,AvvShare,MortShare,tvx,TToFunc
		REAL ::	StemToTScoar,GI(MXSPECI)
        DATA    StemToTScoar/0.027/ ! Stems left on harvesting site
        DATA mmin/5./
        REAL :: CostGROT(mxper), GROTtime
        REAL ::	Densitet(MXSPECI),m3fubTom3sk(MXSPECI)
!       Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch,HybAsp,Poppel
  		DATA Densitet/0.41,0.4,0.49,0.4,0.57,0.6,0.4,0.4,0.4,0.4,0.4,0.4/
  		DATA m3fubTom3sk/1.192,1.158,10*1.26/ ! SKS, RAPPORT 2022/16
        DATA InplantPeriods/4/ ! The maximum number of periods for inplanting from final felling

!-- Start
        irec=0
	  	if(istart == 0 )then
      		write(NR,'(a)')'COMPOSITE TABLE'
      		write(NR,	&
     '(15x,a,6x,a,2x,a,4(1x,a),2x,a,1x,a,1x,a,1x,a,1x,a,1x,a,1x,a,2x,a,2x,a,2x,a,3x,a,1x,a,2x,a,2x,a,2x,a)')	&
     'i','j','t',' NRatg','Treat0','LSA','Age','Hgv','Stems',	&
     'm3Vol','m3Pin','m3Spr','m3Dec',	&
     'm3Harv','SCost','HCost','Rcost','Dgv','M3skMort','TSbiom','TScoar','TSfine'
       		write(70,'(a)')'COMPOSITE TABLE'
      		write(70,'(1x,a,15x,a,6x,a,3x,a,1x,a)')	&
     'Factors','i','j','t','PlotOutput'
     		istart = 1
     	 endif

!-- Initialize
	irec=0
    Ver=VERSION(1:2)
    CstandId = ' '
	i =18-INDEX(CFIX(StandId),' ')
	CstandId(i:16)=CFIX(StandId)

!-- Select for output
    maxThinnings=.false.;Fertilization=.false.;Lodgepole=.false.;CCF=.false.
    if(Ver == 'Fe')then
		Fertilization=.true.
    elseif(Ver == 'TF')then
		maxThinnings=.true.
    elseif(Ver == 'Th')then
		maxThinnings=.true.
    elseif(Ver == 'Co')then
        Lodgepole=.true.
    elseif(Ver == 'CC')then
        CCF=.true.
    endif
! Max 2 thinnings
	if(maxThinnings)then
		j=0
		do iiper=iper1,iper,iper2
    		if(IXATG(nratg(iiper)) == FF )j=0
    		if(IXATG(nratg(iiper)) == Th .or. IXATG(nratg(iiper)) == FT)j=j+1
			if(j>2)return
    	enddo
	endif
! Fertilization
	if(Fertilization)then
		j=0
    	do iiper=iper1,iper,iper2
    		if(IXATG(nratg(iiper)) ==  Fe .or. IXATG(nratg(iiper)) ==  FT)j=j+1
    	enddo
    	if(j == 0)return
	endif
! Contorta
	if(Lodgepole)then
        if(FIX(SI) < 22. .or. FIX(SI) > 26.)return
		j=0
    	do iiper=iper1,iper,iper2
    		if(IXATG(nratg(iiper)) ==  6 )j=j+1
    	enddo
    	if(j == 0)return
		j=0
    	do iiper=iper1,iper,iper2
    		if(IXATG(nratg(iiper)) ==  6 )then
                do i=iiper+1,iper
                  if(IXATG(nratg(iiper)) /= 1 .and. IXATG(nratg(iiper)) /= 6 ) return
                enddo
            endif
    	enddo
	endif
! CCF
	if(CCF)then
		j=0
    	do iiper=iper1,iper,iper2
    		if(IXATG(nratg(iiper)) ==  Cl)j=j+1
    	enddo
    	if(j < 3)return
	endif
! Remove: final harvest without inplanting IP
	do iiper =1,NPER-InplantPeriods
    	if(IXATG(nratg(iiper)) == FF)then
        	j=0
        	do i=iiper,nper
          		if(IXATG(nratg(i)) == IP)j=j+1
        	enddo
            if(j==0)return
        endif
    enddo

!-- non-factor info
     	if( WriteTest )then
			write(nr,'(a16,99f10.1)')	&
            CstandId,FIX(Area),BESTin(V,1),Bestin(V,2),Bestin(V,3),Bestin(V,4),Bestin(V,5)
!          do iiper=iper1,iper,iper2
!			write(nr,'(i10,i4,99f8.2,12(7f6.1))')	&
!     		IFIX(StandID),iiper,BESTin(:,iiper),ARTin(:,:,iiper)
!          enddo
          return
        endif

!-- Initiate new ID or Version of the same ID
	if(Bnr /= CFIX(StandID) .OR. Ver /= Version(1:2))then
       	Pnr=0
        Ver = Version(1:2)
        icp = 0
        j=0
        Bnr=CFIX(StandID)(1:16)
        CurrAct(:) = 0
        FFn(:)=0
    endif

!-- Get resource information
    call G3URES(1,iper,CostSilv,CostHarv,restim,resmav,resreject,IERR)
    if(IERR == 1)RETURN

 !----------- Generate Model II and I programs -------------------------------------
if(ModelII)then
    Rstart(:)=0
    Rstop(:) =0
    if(COUNT(IXATG(nratg(1:iper)) == FF,1) == 0)then
        Rstart(1) = 1
        Rstop(1)  = iper
    else
        F0 = 1
        F1 = iper
        do t=iper,1,-1
            if(IXATG(nratg(t)) == FF)F0 = t
        enddo
        do t=iper,F0+1,-1
            if(IXATG(nratg(t)) == FF .AND. FFn(t) > 0)F1 = t
        enddo
        if(F0 == iper)then
            Rstart(1) = 1
            Rstop(1)  = iper
        else
            if(FFn(F0) == 0)then
                Rstart(2) = 1
                Rstop(2)  = F0
            endif
            Rstart(1) = F0+1
            Rstop(1)  = F1
            if(FFn(F0) > 0 .AND. COUNT(nratg(F0+1:F1) /= CurrAct(F0+1:F1),1) == 0)RETURN
        endif
        FFn(F0) = FFn(F0) +1
        CurrAct(F0:iper) = nratg(F0:iper)

    endif
else
    Rstart(1) = 1
    Rstop(1)  = iper
endif

! Loop for the rotations (Model II) or the entire planning horizon (Model I)
t=1
do t=1,2

    if(Rstart(t) == 0)exit
    
    Pnr = Pnr + 1
    if(Pnr > 9999)then
        write(*,*)'More than 10,000 alternatives generated for one unit'
        stop
    endif
    write(jNr(2:3),'(a)')Version(1:2)
    write(jNr(4:7),'(i4.4)')Pnr
    jNr(1:1)=' '

    do iiper=Rstart(t),Rstop(t),iper2

    !-- Biomass
        TSbiom(iiper) = 0.0	! Holds all biomass in stand
		TScoar(iiper) = 0.0	! Holds all added coarse material (stumps and thick roots and stem witout top)
		TSfine(iiper) = 0.0	! Holds all added fine material (all biomass above stump except stem without top)
		ResStm(iiper) = 0.0	! Holds total harvestable GROT (fine material above stump = all biomass above stump except stem without top)
		ResRot(iiper) = 0.0	! Holds total harvestable stump and thick roots
        call G3ElfvingMeanTree(IFIX,FIX,BESTin(TimeTh,iiper),ARTin(:,:,iiper), GI)	
        DO I=1,MXSPECI
            if(ARTin(Vs,i,iiper) > 0.1)then				! Etablerad skog
               TTm = 0.; TTi = 0.; ACo = 0.; AFi = 0.; BCo = 0.; BFi = 0. 
               TTi = ARTin(Vs,i,iiper)*0.964*Densitet(i)*1.379*1.275  ! Total biomass IPCC, 2003 Equation 3.2.3 
               if(G3DIA(ARTin(Gs,i,iiper),ARTin(Ns,i,iiper)) > 5. )then
                   dia0 = G3DIA(ARTin(Gs,i,iiper),ARTin(Ns,i,iiper))
                   dia1 = G3DIA((GI(I)+ARTin(Gs,i,iiper)),ARTin(Ns,i,iiper))
                   tvx = MAX(0.,(dia1-dia0)/2.)
                   call BIOMASS(I,FIX(ASL),FIX(Lat),FIX(SI),dia0,ARTin(BHAs,i,iiper),tvx, &
        	            TTm,ATo,BCo,Agr,Ast)
                   TTm = TTm*ARTin(Ns,i,iiper)	! Total biomass
                   ATo = ATo*ARTin(Ns,i,iiper)	! Total biomass above
                   BCo = BCo*ARTin(Ns,i,iiper)	! below coarse
                   Agr = Agr*ARTin(Ns,i,iiper)	! above grenar och toppar
                   Ast = Ast*ARTin(Ns,i,iiper)	! stem above
                   ACo =Ast
                   AFi = ATo - ACo
                   BFi = max(0.,TTm - ACo - AFi - BCo)
                   ACo = ACo*TTi/TTm
                   AFi = AFi*TTi/TTm
                   BCo = BCo*TTi/TTm
                   BFi = BFi*TTi/TTm
                          ! BFi = FineRoots(ARTin,IFIX(Peat),FIX(Tsum))/1000.*ARTin(Gs,i,iiper)/BESTin(G,iiper)
                          ! BCo = BelowGrondBiomass(i,dia,ARTin(BHAs,i,iiper))*ARTin(Ns,i,iiper) - BFi	! Coarse biomass below
               else
                   AFi = 0.5*TTi
                   BFi = 0.5*TTi
               endif
               ! Turning into dead material
			   MortShare = ARTin(Ms,i,iiper)/ARTin(Vs,i,iiper)
               AvvShare = CUT(UVs,i,iiper)/ARTin(Vs,i,iiper)
               ACoD = ACo*MortShare
               AFiD = AFi*(MortShare+AvvShare)
               BCoD = BCo*(MortShare+AvvShare)
               BFiD = BFi*(MortShare+AvvShare)
               ! Addition of dead material
               TScoar(iiper) = TScoar(iiper) + ACoD + BCoD
               TSfine(iiper) = TSfine(iiper) + AFiD + BFiD
           	   TSbiom(iiper) = TSbiom(iiper) + ACo + AFi + BCo + BFi
               if(IXATG(nratg(iiper)) == FF .or. IXATG(nratg(iiper)) == Th)then
                   ResStm(iiper) = ResStm(iiper) + AFiD	! Assume only fine material excl. needles in GROT, coarse left on ground or goes to landing
                   ResRot(iiper) = ResRot(iiper) + BCoD
               endif
            endif
        enddo

    !-- Forwarding of residues
        if( ResStm(iiper) > 10. ) then
            CostGROT(iiper) = GROTtime(ResStm(iiper),FIX(mToRoad),53.,10.,0.75)*1070.
        else
            CostGROT(iiper) = 0.
            ResStm(iiper) = 0.
        endif

    ! Forest info for further analysis        
		            write(nr,'(a16,a7,i3,i7,a7,2i4,f5.1,i6,4i6,4i7,f6.1,f9.1,3f8.1)') &
                 CstandId, jNr, iiper,nratg(iiper),CTRT(IXATG(nratg(iiper))),	& 
                 nint(BESTin(LSA,iiper)), 	&
                 nint(BESTin(TotAge,iiper)),	&
                 BESTin(Hdom,iiper),	&
                 nint(BESTin(N,iiper)),	&
                 nint(BESTin(V,iiper)),	&
                 nint( ARTin(Vs,Pine,iiper)+ARTin(Vs,Contorta,iiper) ),	&
                 nint( ARTin(Vs,Spruce,iiper) ),	&
                 nint( BESTin(V,iiper)*BESTin(SpDec,iiper) ),	&
                 nint(sum(cut(UVs,:,iiper))),	&
                 nint(CostSilv(iiper)),	&
                 nint(CostHarv(iiper)),	&
                 nint(CostGROT(iiper)),	&
                 BESTin(D,iiper),	&
                 BESTin(Mort,iiper),	&
                 TSbiom(iiper), &
                 TScoar(iiper), &
                 TSfine(iiper)
		
            ! factor info (excl. cost)
			            if( restim(Pine,iiper) > 1. )	&
                 write(70,'(2x,a,a16,a7,i4,f11.1)')	&
                 'PinTim',CstandId, jNr, iiper,restim(Pine,iiper)
  			            if( restim(Spruce,iiper) > 1. )	&
                 write(70,'(2x,a,a16,a7,i4,f11.1)')	&
                 'SprTim',CstandId, jNr, iiper,restim(Spruce,iiper)
  			            if( sum(restim(3:MXSPECI,iiper)) > 1. )	&
                 write(70,'(2x,a,a16,a7,i4,f11.1)')	&
                 'BerTim',CstandId, jNr, iiper,sum(restim(3:MXSPECI,iiper))	! Assumes that only birch could be timber
  			            if( resmav(Pine,iiper)+ resmav(Contorta,iiper) > 1. )	&
                 write(70,'(2x,a,a16,a7,i4,f11.1)')	&
                 'PinPuw',CstandId, jNr, iiper,(resmav(Pine,iiper) + resmav(Contorta,iiper))
  			            if( resmav(Spruce,iiper) > 1. )	&
                 write(70,'(2x,a,a16,a7,i4,f11.1)')	&
                 'SprPuw',CstandId, jNr, iiper,resmav(Spruce,iiper)
  			            if( sum(resmav(3:MXSPECI,iiper) - resmav(Contorta,iiper)) > 1. )	&
                 write(70,'(2x,a,a16,a7,i4,f11.1)')	&
                 'Decids',CstandId, jNr, iiper,(sum(resmav(3:MXSPECI,iiper)) - resmav(Contorta,iiper) ) 
			            if( ResStm(iiper) > 1. )	&
                 write(70,'(2x,a,a16,a7,i4,f11.1)')	&
                 'ResStm',CstandId, jNr, iiper, ResStm(iiper)
  			            if( ResRot(iiper) > 1. )&
                 write(70,'(2x,a,a16,a7,i4,f11.1)')	&
                 'ResRot',CstandId, jNr, iiper, ResRot(iiper) 

    enddo      
     	
    irec=irec+1

enddo

RETURN
END
