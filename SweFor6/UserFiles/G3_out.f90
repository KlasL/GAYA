      SUBROUTINE G3OUT(NR,M,IPER,IREC)
!***********************************************************************
!  Rutin f”r utskrift av alternativ. Kallas efter  om IERR i denna
!  rutin lika med 0.
!
!  PARAMETRAR:
!     NR    = (in) logiskt nummer (”ppnas mot fil med kommandot FOUT)
!     M     = (in) ordningsnummer p† best†nd/yta f”r vilket ber„kning
!                  skett (svarar mot parameter M i rutin G3GET)
!     IPER  = (in) sista perioden för vilken ber„kningar skett
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
        CHARACTER :: Ver*2,jNr*6,FactorFile*80,Bnr*16,CstandId*22,Cout*2000,VerOut*4
      	real     rtemp(mxper),ctemp(MXSPECI,mxper),AmenityValue
        DATA 	istart,irecSUM,Pnr/3*0/,Bnr/' '/,Ver/' '/
        REAL    :: CostSilv(MXPER),CostHarv(MXPER), &
            restim(MXSPECI,MXPER),resmav(MXSPECI,MXPER),resreject(MXPER)
        REAL	:: hd
		DATA        hd/10./                ! dominant height for switch to establised forest
        CHARACTER   :: CTRT(7)*4
        DATA CTRT/'"NM"', '"IP"', '"Cl"', '"Th"', '"Fe"', '"FT"', '"FF"'/
       logical	:: WriteTest,maxThinnings,Fertilization,Lodgepole,CCF,FirstWrite,ModelII
       integer  :: CurrAct(MXPER),FFn(MXPER),F0,F1
       DATA	WriteTest/.false./,ModelII/.false./,FirstWrite/.true./
       CHARACTER    :: Species(MXSPECI)*8
       data Species/'Pine','Spruce','Birch','Aspen','Oak','Beech', &
           'SouthBrl','Contorta','OtherBrl','Larch','HybAsp','Poppel'/

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
!!!      		write(NR,'(a)')'COMPOSITE TABLE'
!      		write(NR,	&
!     '(13x,a,3x,a,4(1x,a),2x,a,1x,a,1x,a,1x,a,1x,a,1x,a,1x,a,2x,a,2x,a,2x,a,3x,a,1x,a,2x,a,2x,a,2x,a)')	&
!     'j','t',' NRatg','Treatm','LSA','Age','Hgv','Stems',	&
!     'm3Vol','m3Pin','m3Spr','m3Dec',	&
!     'm3Harv','SCost','HCost','Rcost','Dgv','M3skMort','TSbiom','TScoar','TSfine'
     		istart = 1
     	 endif

!-- Select for output
    maxThinnings=.false.;Fertilization=.false.;Lodgepole=.false.;CCF=.false.
    if(Ver(1:1) == 'F')then
		Fertilization=.true.
    elseif(Ver(1:1) == 'T')then
		maxThinnings=.true.
    elseif(Ver(1:1) == 'T')then
		maxThinnings=.true.
    elseif(Ver(1:1) == 'L')then
        Lodgepole=.true.
    elseif(Ver(1:1) == 'C')then
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
    		if(IXATG(nratg(iiper)) ==  Cl .OR. IXATG(nratg(iiper)) == Th)j=j+1
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
     	if( WriteTest .AND. IFIX(StandID) == 9)then ! NB: IFIX(StandID) not used
          do iiper=iper1,iper,iper2
			write(nr,'(3i6,99f10.1)')	&
             IFIX(StandID),iiper,nratg(iiper), &
            FIX(Area),BESTin(V,1),Bestin(V,2),Bestin(V,3),Bestin(V,4),Bestin(V,5)
!			write(nr,'(i10,i4,99f8.2,12(7f6.1))')	&
!     		IFIX(StandID),iiper,BESTin(:,iiper),ARTin(:,:,iiper)
          enddo
          return
        endif

!--
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

!----------- Generate Model II programs -------------------------------------
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
do while(Rstart(t) > 0)

    Pnr = Pnr + 1
    if(Pnr > 9999)then
        write(*,*)'More than 10,000 alternatives generated for one unit'
        stop
    endif
    write(jNr(1:2),'(a)')Version(1:2) 
    write(jNr(3:6),'(i4.4)')Pnr

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
                          BCo = BCo*ARTin(Ns,i,iiper)	! Total biomass above
                          Agr = Agr*ARTin(Ns,i,iiper)	! Total biomass above
                          Ast = Ast*ARTin(Ns,i,iiper)	! Total biomass above
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

!-- Write data for AIMMS and for further analysis
            ! To AIMMS 
            if(FirstWrite)write(NR,'(99a)')	&
        'Plots,Progs,Periods,Version,NRatg,Treatm,', &
        'm3Harv,SCost,HCost,', &
        'Age,m3Vol,Hgv,Dgv,Stems,',	&
        'TSbiom,TScoar,TSfine'
                VerOut = '"'//Version(1:2)//'"'
                write (Cout, '(1x, G, 99(",", G))') &
                CFIX(StandID),jNr,iiper,VerOut,    &
                nratg(iiper),CTRT(IXATG(nratg(iiper))),	& 
                 nint(sum(cut(UVs,:,iiper))), &
                 nint(CostSilv(iiper)),	&
                 nint(CostHarv(iiper)), &
                 nint(BESTin(TotAge,iiper)),	&
                 nint(BESTin(V,iiper)),	&
                 nint(10.*BESTin(Hdom,iiper)),	&
                 nint(10.*BESTin(D,iiper)),	&
                 nint(BESTin(N,iiper)),	&
                 TSbiom(iiper), &
                 TScoar(iiper), &
                 TSfine(iiper)
                k=0
                do i = 1,len(TRIM(Cout))
                        if(Cout(i:i) /=  ' ')then
                            k = k + 1
                            Cout(k:k) = Cout(i:i)
                        endif
                enddo
                write(nr,'(a)')Cout(1:k-1)

            ! factor info (excl. cost)
                if(FirstWrite)then
                    write(70,'(a)')'COMPOSITE TABLE'
      		        write(70,'(a)')	&
                        ' Factors             i      j   t PlotOutput'
                endif
			            if( restim(Pine,iiper) > 1. )	&
                 write(70,'(2x,a,2x,a12,1x,a,i4,f11.1)')	&
                 'PinTim',CFIX(StandID),jNr,iiper,restim(Pine,iiper)
  			            if( restim(Spruce,iiper) > 1. )	&
                 write(70,'(2x,a,2x,a12,1x,a,i4,f11.1)')	&
                 'SprTim', CFIX(StandID),jNr, iiper,restim(Spruce,iiper)
  			            if( sum(restim(3:MXSPECI,iiper)) > 1. )	&
                 write(70,'(2x,a,2x,a12,1x,a,i4,f11.1)')	&
                 'BerTim', CFIX(StandID),jNr, iiper,sum(restim(3:MXSPECI,iiper))	! Assumes that only birch could be timber
  			            if( resmav(Pine,iiper)+ resmav(Contorta,iiper) > 1. )	&
                 write(70,'(2x,a,2x,a12,1x,a,i4,f11.1)')	&
                 'PinPuw', CFIX(StandID),jNr, iiper,(resmav(Pine,iiper) + resmav(Contorta,iiper))
  			            if( resmav(Spruce,iiper) > 1. )	&
                 write(70,'(2x,a,2x,a12,1x,a,i4,f11.1)')	&
                 'SprPuw', CFIX(StandID),jNr, iiper,resmav(Spruce,iiper)
  			            if( sum(resmav(3:MXSPECI,iiper) - resmav(Contorta,iiper)) > 1. )	&
                 write(70,'(2x,a,2x,a12,1x,a,i4,f11.1)')	&
                 'Decids', CFIX(StandID),jNr, iiper,(sum(resmav(3:MXSPECI,iiper)) - resmav(Contorta,iiper) ) 
			            if( ResStm(iiper) > 1. )	&
                 write(70,'(2x,a,2x,a12,1x,a,i4,f11.1)')	&
                 'ResStm', CFIX(StandID),jNr, iiper, ResStm(iiper)
  			            if( ResRot(iiper) > 1. )&
                 write(70,'(2x,a,2x,a12,1x,a,i4,f11.1)')	&
                 'ResRot', CFIX(StandID),jNr, iiper, ResRot(iiper) 
                        
                FirstWrite = .false.

        enddo   ! End period loop Rstart(p) to p

    irec=irec+1

    t = t + 1
    if(t==3)exit
    
enddo

RETURN
END
