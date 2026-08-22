      SUBROUTINE G3OUT(NR,M,IPER,IREC)
!***********************************************************************
!  Rutin f”r utskierrrift av alternativ. Kallas efter  om IERR i denna
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
        CHARACTER :: Ver*2,jNr*7,FactorFile*80,Bnr*16
       CHARACTER :: CstandId*20,Cout*2000,VerOut*4
      	real     rtemp(mxper),ctemp(MXSPECI,mxper),AmenityValue
        real    :: LSAfa
        DATA 	istart,irecSUM,Pnr/3*0/,Bnr/' '/
        REAL    :: CostSilv(MXPER),CostHarv(MXPER),CostForw(mxper),CostGROT(mxper), &
            restim(MXSPECI,MXPER),resmav(MXSPECI,MXPER),resreject(MXPER)
        REAL	:: hd
		DATA        hd/7./                ! dominant height for switch to establised forest
        CHARACTER   :: CTRT(7)*4
        DATA CTRT/'"NM"', '"IP"', '"Cl"', '"Th"', '"Fe"', '"FT"', '"FF"'/
       logical	:: WriteTest,maxThinnings,Fertilization,Lodgepole,CCF,FirstWrite,ModelII,Pilot
       integer  :: CurrAct(MXPER),FFn(MXPER),F0,F1
       DATA	WriteTest/.false./,ModelII/.true./,Pilot/.false./

! Biomass
        REAL :: TSbiom(mxper), TScoar(mxper), TSfine(mxper), ResStm(mxper), ResRot(mxper)
        REAL :: ATo,ACo,BCo,AFi,AGr,Ast,BFi,ACoD,BCoD,AFiD,BFiD,FineRoots,BelowGrondBiomass
        REAL :: TTo,TTm,TTi,TTb,ABa,m3fub
		REAL ::	dia0,dia1,mmin,AvvShare,MortShare,tvx,TToFunc
		REAL ::	StemToTScoar,GI(MXSPECI)
        DATA    StemToTScoar/0.027/ ! Stems left on harvesting site
        DATA mmin/5./
        REAL ::	Densitet(MXSPECI),m3fubTom3sk(MXSPECI)
!       Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch,HybAsp,Poppel
  		DATA Densitet/0.41,0.4,0.49,0.4,0.57,0.6,0.4,0.4,0.4,0.4,0.4,0.4/
  		DATA m3fubTom3sk/1.192,1.158,10*1.26/ ! SKS, RAPPORT 2022/16
        DATA InplantPeriods/4/ ! The maximum number of periods for inplanting from final felling

!-- Start
        irec=0
	  	if(istart == 0 )then
            FirstWrite = .true.
     		istart = 1
        endif

    !-- Initialize
	irec=0
	CstandId=CFIX(StandId)

!-- Select for output
    maxThinnings=.false.;Fertilization=.false.;Lodgepole=.false.;CCF=.false.

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
        !write(*,*)'More than 10,000 alternatives generated for one unit'
        return
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

    !-- Get resource information
        call G3URES(1,iper,ResStm,CostSilv,CostHarv,CostForw,CostGROT,restim,resmav,resreject,IERR)
        if(IERR == 1)RETURN

        ! Forest info for further analysis
        call PrepOutputData(nr,FirstWrite,Pilot,jNr,iiper,CTRT(IXATG(nratg(iiper))), &
            CostSilv(iiper),CostHarv(iiper),CostForw(iiper),CostGROT(iiper), &
            TSbiom(iiper),TScoar(iiper),TSfine(iiper), Cout )
 
        k=0
        do i = 1,len(TRIM(Cout))
            if(Cout(i:i) /=  ' ')then
                k = k + 1
                Cout(k:k) = Cout(i:i)
            endif
        enddo
        write(nr,'(a))')Cout(1:k-1)
		
            ! factor info (excl. cost)
                Decids = SUM(restim(birch:MXSPECI,iiper)) + sum(resmav(birch:MXSPECI,iiper))  &
                    - restim(Birch,iiper) - restim(Oak,iiper) - restim(Poppel,iiper) &
                    - restim(Contorta,iiper) - resmav(Contorta,iiper)
                if(FirstWrite)write(70,'(99a)')'Factor,Plot,Prog,Periods,PlotOutput'
                    if( restim(Pine,iiper) > 1. )	&
                 write(70,'(2x,a,",",a,",",a,",",i4,",",f6.1)')	&
                 'PinTim',trim(CstandId), jNr, iiper,restim(Pine,iiper)
  			            if( restim(Spruce,iiper) > 1. )	&
                 write(70,'(2x,a,",",a,",",a,",",i4,",",f6.1)')	&
                 'SprTim',trim(CstandId), jNr, iiper,restim(Spruce,iiper)
  			            if( restim(Birch,iiper) > 1. )	&
                 write(70,'(2x,a,",",a,",",a,",",i4,",",f6.1)')	&
                 'BerTim',trim(CstandId), jNr, iiper,restim(Birch,iiper)
  			            if( restim(Oak,iiper) > 1. )	&
                 write(70,'(2x,a,",",a,",",a,",",i4,",",f6.1)')	&
                 'OakTim',trim(CstandId), jNr, iiper,restim(Oak,iiper)
  			            if( restim(Poppel,iiper) > 1. )	&
                 write(70,'(2x,a,",",a,",",a,",",i4,",",f6.1)')	&
                 'PopTim',trim(CstandId), jNr, iiper,restim(Poppel,iiper)
  			            if( resmav(Pine,iiper)+ resmav(Contorta,iiper) > 1. )	&
                 write(70,'(2x,a,",",a,",",a,",",i4,",",f6.1)')	&
                 'PinPuw',trim(CstandId), jNr, iiper,(resmav(Pine,iiper) + resmav(Contorta,iiper))
  			            if( resmav(Spruce,iiper) > 1. )	&
                 write(70,'(2x,a,",",a,",",a,",",i4,",",f6.1)')	&
                 'SprPuw',trim(CstandId), jNr, iiper,resmav(Spruce,iiper)
  			            if( Decids > 1. )	&
                 write(70,'(2x,a,",",a,",",a,",",i4,",",f6.1)')	&
                 'Decids',trim(CstandId), jNr, iiper,Decids
			            if( ResStm(iiper) > 1. )	&
                 write(70,'(2x,a,",",a,",",a,",",i4,",",f6.1)')	&
                 'ResStm',trim(CstandId), jNr, iiper, ResStm(iiper)
  			            if( ResRot(iiper) > 1. )&
                 write(70,'(2x,a,",",a,",",a,",",i4,",",f6.1)')	&
                 'ResRot',trim(CstandId), jNr, iiper, ResRot(iiper) 

            FirstWrite = .false.
    enddo      
    irec=irec+1

enddo

RETURN
END
    
SUBROUTINE PrepOutputData(NR,FirstWrite,Pilot,jNr,iiper,CTRT, &
    CostSilv,CostHarv,CostForw,CostGROT, &
    TSbiom,TScoar,TSfine, Cout )

	USE G3_Global
    USE G3_GAMAIN
	USE G3_GAPER
	USE G3_GAFRAM
	USE G3_NAMES 

    integer     :: NR,iiper
    logical     :: FirstWrite,Pilot
    character   :: Cout*2000,jNr*7,CTRT*4
    real        ::CostSilv,CostHarv,CostForw,CostGROT,TSbiom,TScoar,TSfine,LSAfa
   

!-- Local
    character   :: Header*500,CoutA*2000,CoutB*1000
    integer     :: k
    
    Header = ' '
    if(FirstWrite)then
        Header = &
        'Plot,Prog,VariableName,Periods,NRatg,Treatm,'// &
        'SCost,Hcost,Fcost,Rcost,'// &
        'Age,m3Vol,m3dec,Hgv,Dgv,Stems,LSA,m3Harv,'// &
        'TSbiom,TScoar,TSfine,M3skMort,'// &
        'm3Pin,m3Spr,m3Bir,m3Nob,m3Ovr,m3Hyb' 
        k = len(TRIM(Header))
        if(Pilot)Header = Header(1:k)// &
        ',dPin,dSpr,dBir,dNob,dOvr,dHyb,'// &
!        'stPin,stSpr,stBir,stNob,stOvr,stHyb'
        'gPin,gSpr,gBir,gNob,gOvr,gHyb,'// &
        'msPin,msSpr,msBir,msNob,msOvr,msHyb'
        write(nr,'(a)')Header(1:len(TRIM(Header)))
    endif
        
   ! '(1x, 6(G,","), 19(F10.1,","),3(F10.3,","),F4.0)'
    write (CoutA, '(12G,4(f8.0,a),99G10.4)') &
                CFIX(StandID),",", &
                jNr,",", &
                jNr,",", &
                iiper,",", &
                nratg(iiper),",", &
                CTRT,",",	& 
                CostSilv,",",	&
                CostHarv,",",	&
                CostForw,",",	&
                CostGROT,",",    &
                BESTin(TotAge,iiper),",",	&
                BESTin(V,iiper),",",	&
                BESTin(V,iiper) - ( sum(ARTin(Vs,Pine:Spruce,iiper)) + ARTin(Vs,Contorta,iiper) ),",",	&
                BESTin(Hdom,iiper),",",	&
                BESTin(D,iiper),",",	&
                BESTin(N,iiper),",",	&
                BESTin(LSA,iiper),",", &
                sum(cut(UVs,:,iiper)),",", &
                TSbiom,",", &
                TScoar,",", &
                TSfine,",",	&
                BESTin(Mort,iiper),",", &
                ARTin(Vs,Pine,iiper) + ARTin(Vs,Contorta,iiper),",",	&
                ARTin(Vs,Spruce,iiper),",",	&
                ARTin(Vs,Birch,iiper),",",	&
                ARTin(Vs,Oak,iiper) + ARTin(Vs,Beech,iiper),",",	&
                ARTin(Vs,Aspen,iiper) + ARTin(Vs,SouthBrl,iiper) +  ARTin(Vs,OtherBrl,iiper) +  ARTin(Vs,Larch,iiper),",", &
                ARTin(Vs,HybAsp,iiper) + ARTin(Vs,Poppel,iiper)
    if(Pilot)then
            write (CoutB, '(99G10.4)')  &
                G3DIA(ARTin(Gs,Pine,iiper)+ARTin(Gs,Contorta,iiper),ARTin(Ns,Pine,iiper)+ARTin(Ns,Contorta,iiper)),",",	&
                G3DIA(ARTin(Gs,Spruce,iiper),ARTin(Ns,Spruce,iiper)),",",	&
                G3DIA(ARTin(Gs,Birch,iiper),ARTin(Ns,Birch,iiper)),",",	&
                G3DIA(ARTin(Gs,Oak,iiper)+ARTin(Gs,Beech,iiper),ARTin(Ns,Oak,iiper)+ARTin(Ns,Beech,iiper)),",",	&
                G3DIA(ARTin(Gs,Aspen,iiper) + ARTin(Gs,SouthBrl,iiper) +  ARTin(Gs,OtherBrl,iiper) +  ARTin(Gs,Larch,iiper), &
                    ARTin(Ns,Aspen,iiper) + ARTin(Ns,SouthBrl,iiper) +  ARTin(Ns,OtherBrl,iiper) +  ARTin(Ns,Larch,iiper)),",",	&
                G3DIA(ARTin(Gs,HybAsp,iiper)+ARTin(Gs,Poppel,iiper),ARTin(Ns,HybAsp,iiper)+ARTin(Ns,Poppel,iiper)),",",	&
                ARTin(Gs,Pine,iiper) + ARTin(Gs,Contorta,iiper),",",	&
                ARTin(Gs,Spruce,iiper),",",	&
                ARTin(Gs,Birch,iiper),",",	&
                ARTin(Gs,Oak,iiper)+ARTin(Gs,Beech,iiper),",",	&
                ARTin(Gs,Aspen,iiper) + ARTin(Gs,SouthBrl,iiper) +  ARTin(Gs,OtherBrl,iiper) +  ARTin(Vs,Larch,iiper),",", &
                ARTin(Gs,HybAsp,iiper) + ARTin(Gs,Poppel,iiper),",",	&
                ARTin(Ms,Pine,iiper) + ARTin(Ms,Contorta,iiper),",",	&
                ARTin(Ms,Spruce,iiper),",",	&
                ARTin(Ms,Birch,iiper),",",	&
                ARTin(Ms,Oak,iiper) + ARTin(Ms,Beech,iiper),",",	&
                ARTin(Ms,Aspen,iiper) + ARTin(Ms,SouthBrl,iiper) +  ARTin(Ms,OtherBrl,iiper) +  ARTin(Ms,Larch,iiper),",", &
                ARTin(Ms,HybAsp,iiper) + ARTin(Ms,Poppel,iiper)
            Cout = CoutA(1:len(TRIM(CoutA)))//','//CoutB(1:len(TRIM(CoutB)))
    else
            Cout = CoutA(1:len(TRIM(CoutA)))
    endif
    
    return
    end

