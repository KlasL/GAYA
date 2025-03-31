REAL Function BelowGrondBiomass(sp,Dtree,age)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Functions for below-ground biomass of Pinus sylvestris, Picea abies, 
! Betula pendula and Betula pubescens in Sweden
! Petersson & Ståhl 2007
! Case B regr. func. type ii (Case B roots >= 2 mm)
!
! D = diameter (cm)
! BelowGrondBiomass = ton TS per tree
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    USE G3_NAMES
    INTEGER	:: sp
	REAL	:: Dtree,age
BelowGrondBiomass = 0.
if(age < 1.)return

DBH = Dtree*10.
if(sp == Pine)then
	y = 3.62193 &
	+ 11.07117*DBH/(DBH+113.) &
	- 0.05029*DBH/age
elseif(sp == Spruce)then	
	y = 4.69287 &
	+ 10.457*DBH/(DBH+138.) &
	- 0.03057*DBH/age
else
	y = 6.1708 &
	+ 10.01111*DBH/(DBH+225.)
endif

BelowGrondBiomass = EXP(y)*1.e-6

return
END