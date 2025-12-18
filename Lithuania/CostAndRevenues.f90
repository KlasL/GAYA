     SUBROUTINE Resoursces(IATYP,BEST,Cut, Revenue,CostSilv,CostHarv,Timber,Pulpwood,Residues,IERR)
!***********************************************************************
!***********************************************************************
!-- GAYA-DEKLARATIONER
    use G3_Global
    USE G3_NAMES
! Formal parameters
	INTEGER ::	IERR
    REAL    ::  BEST(NBEST),Cut(NCUT,MXSPECI)
    REAL    ::  Revenue,CostSilv,CostHarv, &
                Timber(MXSPECI),Pulpwood(MXSPECI),Residues(MXSPECI)
! Local
	  REAL :: HarvHcostThA,HarvHcostThB,FinalFellingCost,ExtractionCost
 	  REAL :: PlantingCost,CleaningCostA,CleaningCostB
	  DATA	HarvHcostThA/16.8/,HarvHcostThB/11./,FinalFellingCost/8.4/,ExtractionCost/6.7/
	  DATA	PlantingCost/1640./,CleaningCostA/2115./,CleaningCostB/250./
      REAL  :: TimberSpec,PulpwoodSpec,ResiduesSpec,RevenueSpec


      IERR=0

    Revenue=0.0
    CostSilv=0.0
    CostHarv=0.0
    Timber=0.0
    Pulpwood=0.0
    Residues=0.0


!-- No management
	if(IATYP == NM)then
        return

!-- Silviculture - planting and other site preparations
    elseif(IATYP == IP)then
        CostSilv = CostSilv +	PlantingCost +	CleaningCostA
        
!-- RÖJNING
    elseif(IATYP.EQ.Cl)then
        CostSilv = CostSilv +   CleaningCostB
        
!-- Thinning
        ELSEIF(IATYP.EQ.Th.OR.IATYP.EQ.FT)THEN
            DO 30 I=1,MXSPECI
               IF(CUT(UVs,I).GT.1.)THEN
               	ITS=min(3,I)
                CALL UTBYTE(ITS,CUT(UVs,I),CUT(UDs,I),CUT(UHs,I), &
                    TimberSpec,PulpwoodSpec,ResiduesSpec,RevenueSpec)
               if(BEST(TotAge) >= 40.)then
                    CostHarv = CostHarv + (TimberSpec+PulpwoodSpec)* HarvHcostThB
                else
                    CostHarv = CostHarv + (TimberSpec+PulpwoodSpec)* HarvHcostThA
                endif
                CostHarv = CostHarv  + ResiduesSpec*ExtractionCost
     			Timber(I) = Timber(I) + TimberSpec
     			Pulpwood(I) = Pulpwood(I) + PulpwoodSpec
                Residues(I) = Residues(I) + ResiduesSpec
                Revenue = Revenue + RevenueSpec
                ENDIF
30          CONTINUE

!-- Final felling
         ELSEIF(IATYP.EQ.FF)THEN
             DO 60 I=1,MXSPECI
               IF(CUT(UVs,I).GT.1.)THEN
               	ITS=min(3,I)
                CALL UTBYTE(ITS,CUT(UVs,I),CUT(UDs,I),CUT(UHs,I), &
                    TimberSpec,PulpwoodSpec,ResiduesSpec,RevenueSpec)
 				CostHarv = CostHarv	+ (TimberSpec+PulpwoodSpec)*FinalFellingCost
                CostHarv = CostHarv  + ResiduesSpec*ExtractionCost
     			Timber(I) = Timber(I) + TimberSpec
     			Pulpwood(I) = Pulpwood(I) + PulpwoodSpec
                Residues(I) = Residues(I) + ResiduesSpec
                Revenue = Revenue + RevenueSpec
              ENDIF
60          CONTINUE

        ENDIF
 
      RETURN

!-- Not for application

99    IERR=1

      RETURN
      END

      SUBROUTINE UTBYTE(ITS,V,D,H, Timber,Pulpwood,Residues,Revenue)
!--------------------------------------------------------------------
!  Ger utbyte och v„rde. V„rden i f”ljande f„lt anv„nds (ber„knade
!  med Ollas' best†ndsmetod i separat program; f”r tall resp gran):
!        GF    = m3fub gagnvirke/m3sk
!        TAND  = m3fub timmer/m3sk
!        IDIM  = index f”r ing†ng i prislista (toppdiaklass)
!        TF    = toppformtal timmer
!
!        (in) ITS  = tr„dslag (1= tall; 2= gran; 3= annat)
!        (in) V    = volym (m3sk)
!        (in) D    = diameter (cm)
!        (in) H    = h”jd (m)
!        (in) OSA  = o/s-andel som andel av timmer
!--------------------------------------------------------------------
integer :: ireg       
REAL*4 Timber,Pulpwood,Residues,Revenue
      REAL :: mmin(3),tmin(3),Merchantable
      DATA mmin/5.,5.,5./, tmin/13.5,13.5,13.5/

      Timber=0.0
      Pulpwood=0.0
      Residues=0.0
      Revenue=0.0
      
      ireg=2
      CALL VDUB(IREG,ITS,V,D,H,VUB,DUB)
      CALL OLLAB("B",DUB,H,MMIN(ITS),TMIN(ITS), G3,GF,TAND,TDIM,TF)

     Merchantable=0.87*VUB
     if(TDIM > 25.)then
       Timber=0.51*Merchantable
     elseif(TDIM > 13.5)then
       Timber=0.28*Merchantable
     else
       Timber=0.06*Merchantable
     endif
     Pulpwood=Merchantable-Timber

     Residues=0.13*VUB

     if(TDIM > 20.)then
       Revenue=78.*Timber
     else
       Revenue=56.*Timber
     endif
     Revenue=Revenue + 48.*Pulpwood
     Revenue=Revenue + 31.*Residues

      RETURN
      END

      SUBROUTINE VDUB(IREG,ITS,V,D,H,VUB,DUB)
!--------------------------------------------------------------------
!        Ber„kning av diameter och volym under bark med N„slund.
!        Volymsber„kningen ub samma som anv„nds i HUGIN-systemet.
!
!        (in) IREG = Norra Sv=1; S”dra Sv=2
!        (in) ITS  = tr„dslag (1= tall; 2= gran; 3= bj”rk)
!        (in) V    = volym (m3sk)
!        (in) D    = diameter (cm)
!        (in) H    = h”jd (m)
!        (ut) VUB  = volym (m3sk ub)
!        (ut) DUB  = diameter (cm ub)
!--------------------------------------------------------------------
      REAL*4 BARK(2,2,2),VP(4,2,2),VU(4,2,2)

      DATA BARK/0.2,0.10,	&
                0.5,0.05,	&
                0.0,0.15,	&
                0.5,0.05/
      DATA VP/0.09314, 0.03069, 0.002818,  0.00000,	&
              0.12020, 0.01504, 0.023410, -0.06590,	&
              0.10720, 0.02427, 0.007315,  0.00000,	&
              0.11040, 0.01925, 0.018150, -0.04936/
      DATA VU/0.05491, 0.03641, 0.002699,  0.00000,	&
              0.11530, 0.01522, 0.021700, -0.05501,	&
              0.06271, 0.03208, 0.005725,  0.00000,	&
              0.10760, 0.01929, 0.017230, -0.04615/

!-- DEFINIERA INDEX
      NS=IREG
      JTS=ITS
      IF(JTS.EQ.3)JTS=1

!-- DIAMETER UB
      DUB=D*(1.-BARK(2,JTS,NS))-BARK(1,JTS,NS)

!-- VOLYM
      D21=D*D
      D22=DUB*DUB
      H2 =H*H
      V1=VP(1,JTS,NS)*D21+VP(2,JTS,NS)*D21*H+VP(3,JTS,NS)*D*H2+VP(4,JTS,NS)*H2
      V2=VU(1,JTS,NS)*D22+VU(2,JTS,NS)*D22*H+VU(3,JTS,NS)*DUB*H2+VU(4,JTS,NS)*H2
      VUB=V*V2/(V1+0.001)

      RETURN
      END

      SUBROUTINE OLLAB(V,DGV,HGV,MMIN,TMIN,G3,GF,TAND,TDIM,TFORM)
!--------------------------------------------------------------------
!     OLLAs best†ndsvisa metod.Rutinen avser ber„kningen av utbytet
!     f”r ett tr„dslag för ett bestånd (variant = "B") eller träd ( "T" ) 
!     Variabler:DGV=grundytev{gd medeldiameter
!               HGV=    "        medelh”jd
!               MMIN=minsta massavedsdiameter
!               TMIN=minsta timmerdiameter
!               G3=gagnvirkesandel - 3 m massaved
!               GF=gagnvirkesandel - fallande massaved
!               TAND=timmerandel
!               TDIM=timrets medeldiameter
!               TFORM=timrets toppformtal
!--------------------------------------------------------------------
      CHARACTER :: V
      REAL*4 MMIN,TMIN

      IF(DGV-1.LE.MMIN)THEN
         G3=0.14
         GF=0.69
      ELSE
         G3=1.-.86/(DGV-MMIN)
         GF=1.-.31/(DGV-MMIN)
      ENDIF
      if(V == "T")then
         TAND = 1.-0.842/(DGV-2.-TMIN) + 58.2/((DGV-2.)*HGV) - 8.7*TMIN/((DGV-2.)*HGV)
      else
         TAND=.86-.6*TMIN/DGV+.009*DGV-.01*TMIN
      endif
      IF(TAND.LT.0.0)TAND=0.0
      IF(TAND.GT.MIN(G3,GF))TAND=MIN(G3,GF)
      TDIM=3.+.52*DGV+.43*TMIN
      TFORM=1.29-.009*HGV+.003*DGV

      RETURN
      END

 