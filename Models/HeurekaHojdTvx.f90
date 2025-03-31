REAL FUNCTION HeurekaHojd(i,A1,H1) result(HeurekaHojdx)
!***********************************************************************
! Höjdtillväxt enl. Heureka:
! Höjdtillväxt för enskilda träd, beräknade med funktioner för övrehöjdens utveckling 
! Kenneth Nyström
! SLU, Umea
! 2003-12-15
! Version: 0,6
! A1,A2,H1 = Totalålder vid A1 och A1+5, samt höjd
! A1 och A2 ska egentligen vara BHA för björk
!***********************************************************************
USE G3_Global
USE G3_NAMES
INTEGER	:: i
REAL	:: A1,A2,H1
!
INTEGER	:: j,Sp2Sp(MXSPECI)
REAL	:: HeCo(10,4),DS,R,H2
	DATA HeCo/ &
		7395.6,1495.3,  394.,  394.,1495.3,1495.3,1495.3,7395.6,1495.3,7395.6,	& !	B0
		1.7829,1.5978, 1.387, 1.387,1.5978,1.5978,1.5978,1.7829,1.5978,1.7829,	& !	B1
		    25,    10,     7,     7,    10,    10,    10,    25,    10,    25,	& ! B2
		     0,     0,   1.3,   1.3,     0,     0,     0,     0,     0,     0 /	  ! C
		!  Pine Spruc   Birch  Aspen   Oak   Beech  SoBrl  Conto  OtBrl  Larch
    ! Recode from common species order to species order of data here
 ! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch,HybAsp,Poppel
   DATA Sp2Sp/ 1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	4,	4 /
 
    if( H1 == 0. )then
        HeurekaHojdx= 0.1
        return
    endif
    j = Sp2Sp(i)
    A2=A1+5.
    DS = HeCo(j,1)/HeCo(j,3)**HeCo(j,2)
    R  = SQRT( (H1-DS)**2 + 4.*HeCo(j,1)*H1/A1**HeCo(j,2) )
    H2 = (H1+Ds+R)/( 2.+( 4.*HeCo(j,1)/A2**HeCo(j,2) )/(H1-DS+R) ) + HeCo(j,4)

    HeurekaHojdx = H2-H1

	RETURN
    END
    

REAL FUNCTION xHeurekaHojd(i,A1,H1) result(xHeurekaHojdx)
! !!!Veerkar vara fel på parametrarna eller modellen
!***********************************************************************
! Höjdtillväxt enl. Heureka:
! Höjdtillväxt för enskilda träd, beräknade med funktioner för övrehöjdens utveckling 
! Report SLU  Faculty of Forestry 05/02/2010
! Björn Elfving
! Growth modelling in the Heureka system
! 
! A1,A2,H1 = Totalålder vid A1 och A1+5, samt höjd
! A1 och A2 ska egentligen vara BHA för björk
!***********************************************************************
USE G3_Global
USE G3_NAMES
INTEGER	:: i
REAL	:: A1,H1
!
INTEGER	:: j
REAL	:: A2,dh,r,asi(7),beta(7),b2(7),Sp2Sp(MXSPECI)

!       pine    spruce  birch   aspen   beech   oak larch
DATA asi/ 25, 10, 7, 7, 15, 1000, 17.97 /
DATA beta/ 7395.6, 1495.3, 394, 693.2, 4239.3, 8841.4, 1529 /
DATA b2/ -1.7829, -1.5978, -1.387, -0.9771, -1.7753, -1.4317, -1.3451 /

    ! Recode from common species order to species order of data here
    DATA Sp2Sp/ 1,	2,	3,	4,	6,	5,	3,	1,	3,	7,	4,	4 /

    j = Sp2Sp(i)
    A2 = A1+5.
    dh = beta(j)*asi(j)**b2(j)
    r  = SQRT( (H1-dh)**2 + 4.*beta(j)*H1*A1**b2(j) )
    xHeurekaHojdx = (H1+dh+r)/( 2. + (4.*beta(j)*A2**b2(j))/(H1-dh+r) )

    
	RETURN
    END

	REAL FUNCTION HojdYoung(i,SIy,Atot)
!***********************************************************************
! Höjdtillväxt enl. Heureka:
! Elfving, B. (2011d). “The Hugin young stand survey: Database and f
! unctions”, in Growth modelling in the Heureka system. 
! (Umeå: Swedish University of Agricultural Sciences). 
! 
! i 	= species
! Si	= site index (m)
! Atot	= total age 
!***********************************************************************
USE G3_Global
USE G3_NAMES
IMPLICIT NONE
	integer	:: i
    real	:: SIy,Atot

    integer	:: Sp2Sp(MXSPECI)
	real	:: b0,b1,b2,Y0,Y1,SIx
 ! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch,HybAsp,Poppel
   DATA Sp2Sp/ 1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12 /

	SIx=SIy				
    b0=0.; b1=0.; b2=0.
    
	if(i == Pine .or. i ==Contorta)then
	    if(i == Contorta)SIx=0.888+1.336*SIx-0.0094*SIx**2
		b0 =7.	
	    b1=-0.57-0.05*SIx	
    	b2= -0.28+0.0094*SIx	
	elseif(i == Spruce .or. i == Beech)then
		if(i == Beech)SIx=7.4 + 0.755*SIx- 0.00268*SIx**2
    	b0= 6.27+12.1/SIx	
	    b1= -0.262-0.0575*SIx+0.00088*SIx**2	
    	b2= -0.323-0.134*b1	
	elseif(i == Birch .or. i == Oak)then	
    	if(i == Oak)SIx=6.5+0.5*SIx
		b0=  6.836+0.03165*SIx-0.002757*SIx**2	
    	b1= -2.694+0.4937*b0-0.05331*b0**2		
	else	! Aspen and all other species as aspen 
	    b0= 10.024-0.1664*SIx	
    	b1=  -4.093+0.1605*SIx-0.0025*SIx**2		
	endif
      
	Y0=b0+b1*log(Atot)+b2*log(Atot)**2
	Y1=b0+b1*log(Atot+5.)+b2*log(Atot+5.)**2
	HojdYoung= SIx/(exp(Y1)+1.) - SIx/(exp(Y0)+1.)

RETURN
END

REAL FUNCTION T13(i,SIS)
!***********************************************************************
! Time to breast hight (y)
! Based on Elfving, B. (2011d). “The Hugin young stand survey: 
! Database and functions”, in Growth modelling in the Heureka system. 
! (Umeå: Swedish University of Agricultural Sciences). 
!
! i = species (see G3_spec)
! SIS = Site index (m)
!***********************************************************************
    USE G3_Global
    INTEGER :: i
    REAL	:: SIS
    INTEGER :: iSIS,TimeTo13(MXSPECI,21)
    
	iSIS = SIS
	iSIS=max(12,min(32,iSIS))-11
! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch,HybAsp,Poppel,SI
data TimeTo13/ &
19.0,28.8,13.6,10.4,13.2,19.8,10.4,14.8,10.4,10.4,10.4,10.4,  &    ! 12
17.7,26.0,12.8,10.3,12.8,18.8,10.3,13.8,10.3,10.3,10.3,10.3,  &    ! 13
16.5,23.6,12.1,10.1,12.5,17.8,10.1,12.9,10.1,10.1,10.1,10.1,  &    ! 14
15.4,21.6,11.5,10.0,12.1,17.0,10.0,12.1,10.0,10.0,10.0,10.0,  &    ! 15
14.4,19.9,10.9,9.8,11.8,16.3,9.8,11.3,9.8,9.8,9.8,9.8,  &    ! 16
13.5,18.4,10.3,9.5,11.5,15.6,9.5,10.6,9.5,9.5,9.5,9.5,  &    ! 17
12.7,17.2,9.7,9.2,11.2,15.0,9.2,10.0,9.2,9.2,9.2,9.2,  &    ! 18
11.9,16.1,9.2,8.9,10.9,14.4,8.9,9.5,8.9,8.9,8.9,8.9,  &    ! 19
11.2,15.1,8.7,8.5,10.6,13.9,8.5,9.0,8.5,8.5,8.5,8.5,  &    ! 20
10.6,14.2,8.2,8.1,10.3,13.4,8.1,8.6,8.1,8.1,8.1,8.1,  &    ! 21
10.0,13.5,7.7,7.6,10.0,13.0,7.6,8.1,7.6,7.6,7.6,7.6,  &    ! 22
9.4,12.8,7.2,7.1,9.7,12.6,7.1,7.8,7.1,7.1,7.1,7.1,  &    ! 23
8.9,12.2,6.7,6.6,9.5,12.2,6.6,7.4,6.6,6.6,6.6,6.6,  &    ! 24
8.5,11.7,6.2,6.0,9.2,11.9,6.0,7.1,6.0,6.0,6.0,6.0,  &    ! 25
8.0,11.2,5.8,5.5,8.9,11.6,5.5,6.8,5.5,5.5,5.5,5.5,  &    ! 26
7.6,10.8,5.3,5.0,8.7,11.3,5.0,6.6,5.0,5.0,5.0,5.0,  &    ! 27
7.3,10.4,4.9,4.5,8.4,11.0,4.5,6.3,4.5,4.5,4.5,4.5,  &    ! 28
6.9,10.1,4.4,4.0,8.2,10.8,4.0,6.1,4.0,4.0,4.0,4.0,  &    ! 29
6.6,9.8,4.0,3.5,7.9,10.6,3.5,5.9,3.5,3.5,3.5,3.5,  &    ! 30
6.3,9.5,3.6,3.1,7.7,10.4,3.1,5.7,3.1,3.1,3.1,3.1,  &    ! 31
6.0,9.3,3.3,2.7,7.4,10.2,2.7,5.5,2.7,2.7,2.7,2.7/      ! 32

T13 = TimeTo13(i,iSIS)
return
end

	REAL FUNCTION DiaAtBRH(i,N,H)
!***********************************************************************
!Elfving, B. 1982. Hugin´s ungskogsinventering 1976-1979.SLU, Projekt HUGIN, rapport nr 27. 
!Rutin i Heureka:  getPlantDiameter()
!d2=b0 × (h-b1)2× ((∑h2+200)b2  
!d= brösthöjdsdiameter, cm
!h=trädhöjd, m
!bi= koefficienter givet trädslag enligt nedan
!
! i 	= species
! H		= height (m)
!***********************************************************************
USE G3_Global
IMPLICIT NONE
	integer	:: i
    real	:: N,H

    integer	:: Sp2Sp(MXSPECI),its
	real	:: B(3,4)
 ! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch,HybAsp,Poppel
   	DATA Sp2Sp/ 1,	2,	3,	4,	4,	4,	4,	1,	4,	2,	4,	4 /
    DATA B/ &
    96.,1.1,-0.5, &	! Pine, Contorta
    18.,0.9,-0.3, & ! Spruce, all other coniferous
    50.,1.3,-0.5, & ! Birch
    75.,1.2,-0.5/   ! All other broadleaves
    
	its=Sp2Sp(i)
	
DiaAtBRH=SQRT( B(1,its)*(H-B(2,its))**2*(N*H**2+200.)**B(3,its) )
return
end

	REAL FUNCTION DiaYOUNG(i,PCT,ART,IFIX,FIX) result(DiaYOUNGx)
!***********************************************************************
! Fahlvik, N., & Nyström, K. (2006). Models for predicting individual 
! tree height increment and tree diameter in young stands in southern Sweden. 
! 2024-07-06 FIX: Reduce tree diameter growth with 20%
!
! i 		= species
! PCT		= 0 no PCT; 1 = PCT within 5 years; 2 = PCT more then 5 years ago
! DiaYOUNG	= diameter (cm) 
!***********************************************************************
USE G3_Global
USE G3_NAMES
IMPLICIT NONE
	integer	:: i,PCT,IFIX(NFIX)
    real	:: ART(NART,MXSPECI),FIX(NFIX)

    integer	:: its,j
    real	:: DHdata(3,21),lnD
    real	:: ha,hc,veg,Alt,Dat,Distk,Sh2,ShD2,Hh,Ldelsh2,Rh,Na,Natr,Newcl,Oldcl,Poor,Rich

    DATA Dhdata/ &
! Pine, sprucem, birch
1.9001, 0.2787, 1.9801,   &   ! Constant   <-- 1
0., 0., -2.54250,   &   ! 1/(h+1)   <-- 2
0, 4.0119, 0,   &   ! 1/(h+4)   <-- 3
0.00938, 0.01714, 0.02574,   &   ! h−1.2   <-- 4
0.7626, 0.7363, 0.6249,   &   ! ln (h−1.2)   <-- 5
0, 0, -0.02143,   &   ! H h   <-- 6
-0.13850, 0.1565, 0,   &   ! ln(H h )   <-- 7
0.4165, 0.4818, 0.3111,   &   ! R h   <-- 8
-0.00149, -0.00178, 0,   &   ! Sh 2 * 0.01   <-- 9
-0.00071, -0.00028, -0.00046,   &   ! hci   <-- 10
0.06036, 0.03062, 0,   &   ! ldelsh2   <-- 11
-0.15420, -0.09407, -0.16280,   &   ! ln(N)   <-- 12
-0.00434, -0.00409, 0,   &   ! Newcl * H h   <-- 13
0, 0, 0.03551,   &   ! Oldcl   <-- 14
0.05467, 0.05723, 0,   &   ! Distk   <-- 15
-0.03233, 0, 0,   &   ! Poor   <-- 16
0, 0.00959, 0,   &   ! Rich   <-- 17
0.000058, 0.000048, 0.00008,   &   ! (Alt/10)2   <-- 18
-0.05194, -0.03132, 0,   &   ! Natr   <-- 19
0, 0, -0.03824,   &   ! Veg   <-- 20
0.05002, 0.09306, 0/   ! Dat   <-- 21

if(ART(Hs,i) < 1.3)then
  DiaYOUNGx = 0.
  RETURN
endif

!    Variable	Definition and units
ha = ART(Hs,i)			!	Tree height (m)
hc = 0.			!	Distance-independent competition index; sum of the height square (Σh 2) * 0.1 of all trees higher than the subject tree on the plot (100 m2)
ShD2 = 0.		! 	Sh2 for deciduous trees
Sh2 = 0.			!	Density, sum of the height square (Σh 2) of all trees on the plot (m2/100 m2)
do j=1,MXSPECI
  if(ART(Hs,j) > 0)then
	hc= hc + ART(Hs,j)*ART(Hs,j)*ART(Ns,j)*max(1.,(ART(Hs,j)-ART(Hs,i))/ART(Hs,i))*0.1/100.
    if(j > Spruce)ShD2 = ShD2 + ART(Hs,j)*ART(Hs,j)*ART(Ns,j)/100.
    Sh2 = Sh2 + ART(Hs,j)*ART(Hs,j)*ART(Ns,j)/100.
  endif	
enddo
veg = 0.			!	Indicator variable for vegetatively propagated birches; veg=number of stems from the same stump, otherwise veg=0
Alt = FIX(ASL)			!	Altitude above sea level (m)
Dat = 0.			!	Indicator variable for plots inventoried before 20 July
Distk = 0.		!	Indicator variable for plots near the sea coast; Distk=1 if the plot is within 5 km of the coast, otherwise Distk=0
Hh = Sh2/sum(ART(Hs,:)*ART(Ns,:))*100.			!	Height-weighted mean height, Σh 2/Σh, of all trees on the plot (m)
Na = sum(ART(Ns,:))			!	Number of stems with a height ≥1.3 m (ha−1)
Ldelsh2 = ShD2/Sh2		!Proportion of broadleaved trees, Σh 2 (broadleaves) of total Sh 2
Rh = ART(Hs,i)/Hh			!	Relative height, defined as the ratio between h and H h
Natr = 0.	!	Indicator variable for natural regenerations; Natr=1 if the regeneration method is natural regeneration, otherwise Natr=0
Newcl = 0.; Oldcl = 0.
if(PCT == 2)then
	Oldcl = 1.		!	Indicator variable for precommercially thinned plots; Oldcl=1, if the plot was cleaned more than 5 years before the measurements, otherwise Oldcl=0
elseif(PCT == 1)then
	Newcl =1./(1+5.)		!	Indicator variable for precommercially thinned plots; Newcl=1/(1 + clyrs) if the plot was cleaned within 5 years before the measurements, otherwise Newcl=0
endif
Poor = 0.; Rich = 0.
if(IFIX(Ftype) >= 15)then
	Poor = 1. 		!	Indicator variable indicating poor nutrient status; Poor=1 if the ground vegetation type is a dwarf-shrub type (types other than Vaccinium myrtillus) or a lichen-cover type, otherwise Poor=0
elseif(IFIX(Ftype) <= 7)then
	Rich = 1. 		!	Indicator variable indicating rich nutrient status; Rich=1 if the ground vegetation type is a herb type or broad grass type, otherwise Rich=0
endif

its=i
if(i > Spruce)its=3
if(i > Contorta)its=1 

lnD = DHdata(its,1)
lnD = lnD + DHdata(its,2)*1./(ha+1.)
lnD = lnD + DHdata(its,3)*1./(ha+4.)
lnD = lnD + DHdata(its,4)*(ha-1.2)
lnD = lnD + DHdata(its,5)*LOG(ha-1.2)
lnD = lnD + DHdata(its,6)*Hh
lnD = lnD + DHdata(its,7)*LOG(Hh)
lnD = lnD + DHdata(its,8)*Rh
lnD = lnD + DHdata(its,9)*Sh2*0.01
lnD = lnD + DHdata(its,10)*hc
lnD = lnD + DHdata(its,11)*Ldelsh2
lnD = lnD + DHdata(its,12)*LOG(Na)
lnD = lnD + DHdata(its,13)*Newcl*Hh
lnD = lnD + DHdata(its,14)*Oldcl
lnD = lnD + DHdata(its,15)*Distk
lnD = lnD + DHdata(its,16)*Poor
lnD = lnD + DHdata(its,17)*Rich
lnD = lnD + DHdata(its,18)*(Alt/10.)**2
lnD = lnD + DHdata(its,19)*Natr
lnD = lnD + DHdata(its,20)*Veg
lnD = lnD + DHdata(its,21)*Dat

DiaYOUNGx = exp(lnD)*0.8	! FIX to reduce growth

RETURN
END
    

