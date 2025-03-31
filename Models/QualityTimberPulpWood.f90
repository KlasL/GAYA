REAL FUNCTION QualityTimberPulpWood(Species,Height)
! Function for transfer share of timber to pulpwood 
! due to (a)hooks, branchesness etc., and 
! (b) increasing deterioration of hardwoods with time
! Species: 1-12 system= Pine -- Poppel
! Height (m)
    USE G3_NAMES
Implicit none

INTEGER	:: Species
REAL	:: Height

REAL	:: basicTtoP	! Reduction due to hooks, branchesness etc.
REAL	:: ageTtoP		! Reduction due to deterioration of with time
REAL	:: Hlimit		! Height when age related deterioration has reached 50%
DATA Hlimit/20./

  basicTtoP = 0.
  if(Species > Spruce .and. Species < HybAsp)basicTtoP=1.
  if(Species == Birch)basicTtoP=0.6
  if(Species == HybAsp .or. Species == Poppel)basicTtoP=0.1

  ageTtoP=0.
  if(Species > Spruce .and. Species /= Contorta)then
	ageTtoP = 1./(1.+EXP(-0.5*(Height-Hlimit)))
  endif
  
QualityTimberPulpWood = basicTtoP + (1.-basicTtoP)*ageTtoP

return
end