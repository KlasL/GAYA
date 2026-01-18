REAL function LSAfa(PoS,SIS,ART) result(LSAfax)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Minimum allowed final harvest age according to the Forestry Act
!
! PoS = part of Sweden (1-3) (1= n:a Sv, 2= m:a Sv, 3= s:a Sv)
! SIS = site index (m)
! ART = species data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    USE G3_NAMES
	USE G3_Global
!
	REAL 	:: SIS,ART(NART,MXSPECI),TotVol,ConifShare,NoblesShare
    INTEGER	::	PoS
    INTEGER	::	iSIS
    real    :: tableLSA(7)
    data tableLSA / 90., 80., 70., 65., 60., 50., 45./
    
    TotVol = sum(ART(Vs,:))
    if(TotVol < 1.)return
    
    iSIS = min(max(int((SIS-12.)/4.)+1,1),7)
    ConifShare = (ART(Vs,Pine) + ART(Vs,Spruce))/TotVol
    NoblesShare = (ART(Vs,Oak) + ART(Vs,Beech))/TotVol

    if(NoblesShare > 0.5)then
    	LSAfax = 90.
    elseif(ConifShare > 0.5)then
        LSAfax = tableLSA(iSIS)
        if(PoS == 1)LSAfax = LSAfax + 10.
    else
        LSAfax = 35.
    endif

    return
    END
    
