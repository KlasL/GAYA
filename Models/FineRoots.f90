REAL Function FineRoots(ART,pe,tsumma)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Modelling fine root biomass of boreal tree stands using site and stand variables
! A. Lehtonen, M. Palviainen, P. Ojanen, T. Kalliokoski, P. Nöjd, M. Kukkola, 
! T. Penttilä, R. Mäkipää, J. Leppälammi-Kujansuu, H.-S. Helmisaari b
!
! FineRoots = kg/ha (root diameter <= 2mm)
! pe = peat
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    USE G3_NAMES
	USE G3_Global
INTEGER	:: pe
REAL	:: ART(NART,MXSPECI),tsumma,y,Gsum
!
FineRoots = 0.
Gsum = sum(ART(GS,:))
if(Gsum < 0.1)return
y = 7.011		& ! Konst.	
+0.605*log(Gsum)	& ! ln(G)	m2/ha
+0.501*ART(Gs,Birch)/Gsum	& ! birch dominated stand
-2.772*pe		& ! peatland
-0.001*tsumma		& ! temperature sum (dd)'
+0.002*pe*tsumma	! pe*tsum

FineRoots = EXP(y)

return
END	
