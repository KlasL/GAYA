REAL function LSAfa(PoS,SIS,ART) result(LSAfax)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Minimum allowed final harvest age according to the Forestry Act
!
! PoS = part of Sweden (1-3)
! SIS = site index (m)
! ART = species data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    USE G3_NAMES
	USE G3_Global
!
	REAL 	:: SIS,ART(NART,MXSPECI),TotVol,ConifShare,NoblesShare
    INTEGER	::	PoS
    
	LSAfax = 35.
    TotVol = sum(ART(Vs,:))
    if(TotVol < 1.)return
    ConifShare = (ART(Vs,Pine) + ART(Vs,Spruce))/TotVol
    NoblesShare = (ART(Vs,Oak) + ART(Vs,Beech))/TotVol

	if(ConifShare > 0.5)then
		if(SIS >= 30.)then
    		LSAfax = 50.
	    elseif(SIS <= 12.)then
    	  	LSAfax = 90.
	    else
    	  	LSAfax = 90.-40./18.*(SIS-12.)
	    endif
	    if(PoS == 1)LSAfax = LSAfax + 10.
    elseif(NoblesShare > 0.5)then
    	LSAfax = 90.
    endif

    return
    END
    
