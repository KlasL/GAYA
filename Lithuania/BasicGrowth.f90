    SUBROUTINE G3TVX(IPER)
!***********************************************************************
!  BESTŽMMER UTGENDE TILLSTND EFTER EN FEMRSPERIOD.
!  FRN UPPGIFTER OM TGŽRD (ID, ATG, CUT), FASTA UPPGIFTER (FIX) OCH
!  IGENDE TILLSTND (IART1, BEST1, ART1) G™RS TILLVŽXTBERŽKNING F™R
!  EN FEMRSPERIOD OCH TILLDELAS SAMTLIGA FŽLT I IART2, BEST2 OCH
!  ART2 NYA VŽRDEN.
!
!  PARAMETRAR:
!     IPER  = (in) aktuell period
!     ID    = (in) †tg„rdskod
!     ATG   = (in) †tg„rdsdefinition
!     CUT   = (in) uppgifter om avverkningsuttag
!     FIX   = (in) fixa data f”r tillv„xtfunktion
!     IART1 = (in) tr„dslagskoder f”r motsvarande f„lt i CUT och ART1
!     BEST1 = (in) best†ndsdata
!     ART1  = (in) tr„dslagsvisa data
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

!-- NOLLSTŽLL
BESTin(:,IPER+1) = 0.
ARTin(:,:,IPER+1)= 0.

!-- Growth projection ----------------------

 	    do i=1,MXSPECI
                ARTin(Ms,i,IPER+1)=0.           
                ARTin(Ns,i,IPER+1)=ARTout(Ns,i,IPER)*RelGr(Ns,i,IPER)           
                ARTin(Gs,i,IPER+1)=ARTout(Gs,i,IPER)*RelGr(Gs,i,IPER)           
                ARTin(Vs,i,IPER+1)=ARTout(Vs,i,IPER)*RelGr(Vs,i,IPER)           
                ARTin(Hs,i,IPER+1)=ARTout(Hs,i,IPER)*RelGr(Hs,i,IPER)           
			    if(ARTin(Ns,i,IPER+1) > 0.)ARTin(BHAs,i,IPER+1)=ARTout(BHAs,i,IPER)+5.
        enddo

RETURN
END
 