SUBROUTINE BIOMASS(i,altitud,bgr,si,d,behald,tvx, tvtot,tvovstub,tvrotstu,tvgren,tvstam)
!----------------------------------------------------------------------
! Computes the biomass based on Markkund's functions (1987)
! 
! Input:
! 	i	= species (1 pine; 2 spruce; >= 3 goes as birch
!     altitud	= altitude (m)
!     bgr	= latitude (o)
!     d	= diameter (cm)
!     bhald	= age at brest hight
!     tvx = radiell tillväxt 5 år (cm)
! 
! Output on tree dry matter weight:
! 	1. Total biomass (TonTS)
! 	2. What can be extracted above ground  (TonTS)
! 	3. What can be extracted below ground  (TonTS)
! 
! What is left on harvesting site for deacay:
! Total biomass (TonTS) 
! - extracted above ground*extraction rate above 
! - extracted below groun*extraction rate below
! 
! Dependant variables
! altitud	m	
! konstant		
! ln(behald)	år	brh
! ln(dbrh)	mm	
! ln(tilvfem)	0.1 mm	radie = dbrh/behald/2*10
! mark	peat = 1	
! nko	grad * 1000
! 	
!----------------------------------------------------------------------
integer ::	i
real ::		altitud,bgr,si,d,behald, tvtot,tvovstub,tvrotstu,tvgren,tvbark,tvstam

real::		mark,nko,dbrh,tilvfem

tvtot = 0.0
tvovstub = 0.0
tvrotstu = 0.0
tvbark = 0.0
tvstam = 0.0
if(d < 0.24) return

mark = 0.
nko = bgr*1000.
dbrh = d*10.
tilvfem = max(1.,tvx*100.)

if(i == 1)then ! Tall
	tvstam = 	-7.674621 &			! T10
    	+ 3.155671*LOG(dbrh+25) &
        - 0.002197*dbrh &
        + 0.084427*LOG(tilvfem) &
        - 0.002665*tilvfem &
		+ 0.253227*LOG(behald) &
		+ 0.031435*si &
		+ 0.000008342*nko

	tvtot = 	-1.507568 &					! T16
			+	2.449121 * LOG(dbrh+5) &
			+	0.104243 * LOG(behald)  &
			-	0.000321 * altitud

	tvovstub = -2.032666 &					! T15
			+	2.413856 * LOG(dbrh+6) &
			+	0.130304 * LOG(behald) &
			+	0.013668 * si
    tvgren =   -2.53322 & !                  T13
            +   1.989129 * LOG(dbrh) &
            +   0.387203 * LOG(tilvfem) &
            +   0.105315 * LOG(behald)
    tvbark =   -1.340149 & !                  T11
            +   2.209719 * LOG(dbrh+13) &
            -   0.001986 * LOG(tilvfem) &
            -   0.000024146 * nko
elseif(i == 2) then ! Gran
	tvstam = - 6.83931 &			! G1
    	+ 3.578450*LOG(dbrh+25) &
		- 0.003042*dbrh &
		+ 0.093033*LOG(tilvfem) &
		- 0.002763*tilvfem &
		+ 0.111347*LOG(behald) &
		+ 0.012148*si &
		- 0.000020194*nko

	tvtot =    -0.614093 &					! G7
			+	2.425997 * LOG(dbrh+8) &
			+	0.081636 * LOG(tilvfem) &
			+	0.128027 * LOG(behald)  &
			-	0.00001581 * nko
    tvovstub = -0.437361 &					! G6
			+	2.446692 * LOG(dbrh+9) &
			+	0.065779 * LOG(tilvfem) &
			+	0.10229  * LOG(behald)  &
			-	0.000021633 * nko
    tvgren =   -0.718621 &                  ! G4
            +   1.74081  * LOG(dbrh) &
            +   0.348379 * LOG(tilvfem) &
            +   0.180503 * LOG(behald)
    tvbark =   -4.084706 & !                  G2
            +   2.397166 * LOG(dbrh+10) &
            -   0.066053 * LOG(tilvfem) &
            +   0.151696 * LOG(behald)
else ! Övrigt
	tvstam = -3.091932 &			! B18
		+ 2.479648*LOG(dbrh+7) &
		+ 0.243747*LOG(behald) &
		+ 0.022185*si
  
	tvtot = 	-0.614093 &					! G7
			+	2.425997 * LOG(dbrh+8) &
			+	0.081636 * LOG(tilvfem) &
			+	0.128027 * LOG(behald)  &
			-	0.00001581 * nko
    tvovstub= -0.423749 &				! B22
			+	2.574575 * LOG(dbrh+8) &
			+	0.090419 * LOG(behald)  &
			-	0.000026862 * nko
    tvgren =   -2.782537 &                  ! B20
            +   2.276815 * LOG(dbrh) &
            +   0.228528 * LOG(tilvfem)
    tvbark =   -3.24449 & !                  B19
            +   2.52542 * LOG(dbrh+18) &
            +   0.329744 * LOG(behald) &
            -   0.00003018 * nko
endif

trsl = 0
if(i == 1) trsl = 1
tvrotstu = -1.980469 &						! GT9
		+	2.339527 * LOG(dbrh) &
		+	0.342786 * mark  &
		-	0.224812 * trsl

tvtot = EXP(tvtot)*1.e-6
tvovstub = EXP(tvovstub)*1.e-6
tvrotstu = EXP(tvrotstu)*1.e-6
tvgren = EXP(tvgren)*1.e-6
tvbark=EXP(tvbark)*1.e-6
tvstam=EXP(tvstam)*1.e-6

RETURN
END