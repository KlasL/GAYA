    SUBROUTINE G3TVX(IPER)
!***********************************************************************
!  BESTéMMER UTGèENDE TILLSTèND EFTER EN FEMèRSPERIOD.
!  FRèN UPPGIFTER OM èTGéRD (ID, ATG, CUT), FASTA UPPGIFTER (FIX) OCH
!  IGèENDE TILLSTèND (IART1, BEST1, ART1) GôRS TILLVéXTBERéKNING FôR
!  EN FEMèRSPERIOD OCH TILLDELAS SAMTLIGA FéLT I IART2, BEST2 OCH
!  ART2 NYA VéRDEN.
!
!  PARAMETRAR:
!     IPER  = (in) aktuell period
!     ID    = (in) ÜtgÑrdskod
!     ATG   = (in) ÜtgÑrdsdefinition
!     CUT   = (in) uppgifter om avverkningsuttag
!     FIX   = (in) fixa data fîr tillvÑxtfunktion
!     IART1 = (in) trÑdslagskoder fîr motsvarande fÑlt i CUT och ART1
!     BEST1 = (in) bestÜndsdata
!     ART1  = (in) trÑdslagsvisa data
!     IART2, BEST2, ART2 = (ut) df som IART1, BEST1 och ART1
!***********************************************************************
!-- GAYA-DEFINITIONER
        USE G3_Global
        USE G3_NAMES
        USE G3_GAFRAM
        use Lt_GrowthData
implicit none      
        INTEGER :: IPER
!-----------------------------------------------------------------------
    integer     :: i

!-- NOLLSTéLL
BESTin(:,IPER+1) = 0.
ARTin(:,:,IPER+1)= 0.

!-- Growth projection ----------------------
    if(IPER < StartToUse)then
        ARTin(:,:,IPER+1) = OrigonalARTin(:,:,IPER+1)
    else
 	    do i=1,MXSPECI
                ARTin(Ms,i,IPER+1)=0.           
                ARTin(Ns,i,IPER+1)=ARTout(Ns,i,IPER)*RelGr(Ns,i,IPER)           
                ARTin(Gs,i,IPER+1)=ARTout(Gs,i,IPER)*RelGr(Gs,i,IPER)           
                ARTin(Vs,i,IPER+1)=ARTout(Vs,i,IPER)*RelGr(Vs,i,IPER)           
                ARTin(Hs,i,IPER+1)=ARTout(Hs,i,IPER)*RelGr(Hs,i,IPER)           
			    if(ARTin(Ns,i,IPER+1) > 0.)ARTin(BHAs,i,IPER+1)=ARTout(BHAs,i,IPER)+5.
        enddo
    endif

RETURN
END
 