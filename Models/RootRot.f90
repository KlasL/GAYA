REAL function RootRot(Spec,Height,Age,Diameter) result(RootRotx)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Function for assessing the probability of a tree with root rot at height 1.3 m
! 
! Johansson, Lars, 2000. Rotröta i Sverige enligt Riksskogstaxeringen : 
! en beskrivning och modellering av rötförekomst hos gran och björk. 
! Second cycle, A1E. Umeå: SLU, Dept. of Forest Resource Management

! Species: 1-12 system= Pine -- Poppel
! Height (m)
! Age
! Diameter (cm)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER	:: Spec,Sp
REAL	:: Height,Age,Diameter
REAL	:: KoeffRR(3,5),func,Hdm,Dmm

! Riksnivå
! Tall,Gran,Björk
DATA KoeffRR/ &
-6.63454711,-8.75831313,-4.919113281,	& ! Konstant
-0.00877,0.00988,-0.0109,				& ! Höjd
0.0161,0.00477,0.0146,					& ! Beståndsålder
0.00952,0.0222,0.0216,					& ! Diameter
0,-0.00002,-0.00002/					 ! Diameter 2

Sp = 1		! All species as pine except spruce and birch
if(Spec == 2)Sp=2
if(Spec == 3)Sp=3
  
Hdm = 10.*Height
Dmm = 10.*Diameter
func = KoeffRR(Sp,1) + &
KoeffRR(Sp,2)*Hdm + &
KoeffRR(Sp,3)*Age + &
KoeffRR(Sp,4)*Dmm + &
KoeffRR(Sp,5)*Dmm*Dmm

RootRotx = EXP(func)/(1.+EXP(func))

return
END