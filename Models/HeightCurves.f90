REAL FUNCTION HeightCurveRegr(i,A1,HK)
!***********************************************************************
! Höjdutvecklingskurvor enl. fakta skog 14, 2013
! converted to regression functions with curve as output
! i = trädslag enl. G3_spec
! A1 = totalålder
! HK = SI (m) - bonitetsbestämmande höjkurva
!
! HeightCurve = (m) höjd enligt höjdkurva
!***********************************************************************
    USE G3_Global
    USE G3_NAMES
INTEGER	:: i
REAL	:: A1,HK
!
REAL	:: A1p(11),SqA1p(11),HKp(11),A1Hkp(11),C(11)
REAL    :: A2(11),p
INTEGER :: Sp2Sp(MXSPECI),j

! Recode from common species order to species order of data here
! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch,HybAsp,Poppel
DATA Sp2Sp/ 2,	1,	6,	9,	5,	4,	9,	2,	9,	3,	10,	11 /
DATA A2/ 97., 100., 100., 100., 100., 50., 50., 50., 50., 50., 50. / ! SI age Hx m


DATA A1p/ &
 -0.27662, -0.28632, -0.37603, -0.23345, -0.25513, -0.34765, -0.34621, -0.34400, -0.19446,  -0.82954, -0.85918 /   ! A1
DATA SqA1p/ &
 7.90594, 7.65567, 8.24996, 6.80313, 5.45747, 7.30943, 7.28527, 6.94461, 3.11165,  14.99362, 14.31010 /   ! Sq(A1)
DATA HKp/ &
 0.95010, 0.95988, 0.83196, 0.80980, 0.61458, 0.95474, 0.96907, 1.03289, 0.27710,  1.25014, 1.18633 /   ! HK
DATA A1Hkp/ &
 0.00045, 0.00024, 0.00187, 0.00181, 0.00354, 0.00065, 0.00038, -0.00081, 0.01425,  -0.00486, -0.00357 /   ! A1*HK
DATA C/ &
 -51.12784, -47.47059, -45.40390, -44.44043, -28.41190, -34.00047, -33.90907, -31.64472, -12.06184,  -64.57493, -58.24851 /   ! C
! Gran, Tall, Lärk, Bok, Ek, Björk, Klibbal, Gråal, Asp,  Hybridasp, Poppel

    
    j = Sp2Sp(i)
    p = 1.
    if( A2(j) < 100. )then
       p =  ( A1p(j)*A2(j) + SqA1p(j)*SQRT(A2(j)) + HKp(j)*HK + A1Hkp(j)*A2(j)*HK + C(j) ) / &
            ( A1p(j)*100. + SqA1p(j)*SQRT(100.) + HKp(j)*HK + A1Hkp(j)*100.*HK + C(j) )
       p =  ( A1p(j)*A2(j) + SqA1p(j)*SQRT(A2(j)) + HKp(j)*HK*p + A1Hkp(j)*A2(j)*HK*p + C(j) ) / &
            ( A1p(j)*100. + SqA1p(j)*SQRT(100.) + HKp(j)*HK + A1Hkp(j)*100.*HK + C(j) ) * p
    endif
	HeightCurveRegr = ( A1p(j)*A1 + SqA1p(j)*SQRT(A1) + HKp(j)*HK*p + A1Hkp(j)*A1*HK*p + C(j) )

	RETURN
    END
    
REAL FUNCTION HeightCurve(i,A1,hd)
!**************************hd fakta skog 14, 2013
! i = trädslag enl. G3_spec
! A1 = totalålder
! hd = höjd på höjdutvecklingskurva
! 
! HeightCurve = SI (m) - bonitetsbestämmande höjkurvaHK
!***********************************************************************
    USE G3_Global
    USE G3_NAMES
INTEGER	:: i
REAL	:: A1,hd
!
REAL	:: A2(11),asi(11),beta(11),b2(11)
REAL    :: ds,r,Z0,p,A1x
INTEGER :: Sp2Sp(MXSPECI),j

! Recode from common species order to species order of data here
DATA Sp2Sp/ 2,	1,	6,	9,	5,	4,	9,	2,	9,	3,	10,	11 /
! Gran, Tall, Lärk, Bok, Ek, Björk, Klibbal, Gråal, Asp,  Hybridasp, Poppel
DATA A2/ 100., 100., 100., 100., 100., 50., 50., 50., 50., 50., 50. / ! SI age Hx m
DATA asi/ 10, 25, 17.97, 15, 1000, 7, 7, 7, 7, 2.0381, 2.1405 /
DATA beta/ 1495.3, 7395.6,  1529, 4239.3, 8841.4, 394, 381.5, 278.9, 693.2, 4692.5, 6460.5 /
DATA b2/ -1.5978, -1.7829, -1.3451, -1.7753, -1.4317, -1.3870, -1.3823, -1.3152, -0.9771, 23.1758, 18.2238 /

    A1x = A1
!    if(i == Spruce)A1x = max(1.,A1 - 3.)
!    if(i == Birch)A1x = max(1.,A1 - 8. )
    j = Sp2Sp(i)

    ds = beta(j) * asi(j)**b2(j)
    r = Sqrt( (hd - ds)**2 + 4. * beta(j) * hd * A1x**b2(j))
    HeightCurve = (hd + ds + r) / (2. + 4. * beta(j) * A2(j)**b2(j) / (hd - ds + r))
    if (i == HybAsp .or. i == Poppel) then 
        Z0 = hd - b2(j);
        P = Z0 + Sqrt(Z0**2 + 2 * beta(j) * hd / A1x**asi(j));
        HeightCurve = hd*((A2(j)**asi(j) * (A1x**asi(j)*P + beta(j))) / (A1x**asi(j)*(A2(j)**asi(j)*P + beta(j)))); 
    endif

return
end


REAL FUNCTION HeightCurve10m(i,SIS)
!***********************************************************************
! Height development curves (fakta skog 14, 2013) regressed for total
! age to reach 10m dominant height
!
! i = species (see G3_spec)
! SIS = Site index (m)
! 
! HeightCurve10m = Total age at which 10 m dominant height is achieved
!***********************************************************************
    USE G3_Global
    INTEGER :: i
    REAL    :: SIS
!
    INTEGER :: Sp2Sp(MXSPECI),its
    REAL    :: Age10regr(3,MXSPECI)
    DAta Age10regr/          & 
!   SQRT(SI)    SI  Const
    -109.24,8.71,359.98,     & ! 1
    -85.73,6.15,314.52,      & ! 2 
    -65.05,5.37,211.58,      & ! 3 
    -58.23,4.81,187.79,      & ! 4 
    -156.80,13.83,461.92,    & ! 5
    -80.39,5.63,299.84,      & ! 6
    -70.63,6.02,214.81,      & ! 7
    -109.24,8.71,359.98,     & ! 8
    -70.63,6.02,214.81,      & ! 9
    -115.89,9.33,374.80,     & ! 10
    -53.22,4.16,178.25,      & ! 11
    -64.95,5.44,202.36/       ! 12
    ! Recode from common species order to species order of data here
    !           1   2   3   4   5   6   7   8   9  10  11  12
    DATA Sp2Sp/ 1,	2,	1,	4,	6,	5,	4,	8,	4, 10, 11, 12 /

    its = Sp2Sp(i)
    HeightCurve10m = Age10regr(1,its)*SQRT(SIS) + Age10regr(2,its)*SIS + Age10regr(3,its)

    RETURN
    END

    REAL FUNCTION HeightCurve15m(i,SIS)
!***********************************************************************
! Height development curves (fakta skog 14, 2013), for age to reach 15m
! dominant height. Value computed with program HeightCurveTest. 
!
! i = species (see G3_spec)
! SIS = Site index (m)
! 
! HeightCurve15m = Total age at which 15 m dominant height is achieved
!***********************************************************************
    USE G3_Global
    INTEGER :: i
    REAL	:: SIS
    INTEGER :: iSIS,Hdom15(MXSPECI,21)
    
	iSIS = SIS
	iSIS=max(12,min(32,iSIS))-11

! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch,HybAsp,Poppel,SI
data Hdom15/ &
131,125,69,65,167,123,65,131,65,133,65,71, & !12,,
120,116,62,60,137,115,60,120,60,121,60,64, & !13,,
110,108,59,55,116,107,55,110,55,110,55,57, & !14,,
101,101,54,50,101,100,50,101,50,101,50,51, & !15,,
92,94,49,47,89,94,47,92,47,92,46,45, & !16,,
85,88,44,44,79,89,44,85,44,85,43,40, & !17,,
78,82,40,41,72,83,41,78,41,78,39,35, & !18,,
72,77,37,38,65,78,38,72,38,72,36,32, & !19,,
66,73,34,36,60,74,36,66,36,66,33,28, & !20,,
61,68,31,34,55,69,34,61,34,61,30,26, & !21,,
56,64,28,32,51,65,32,56,32,56,27,23, & !22,,
52,60,26,30,48,61,30,52,30,52,25,21, & !23,,
48,56,24,28,45,57,28,48,28,48,23,19, & !24,,
45,53,22,27,42,54,27,45,27,45,21,18, & !25,,
42,49,20,26,40,51,26,42,26,42,19,17, & !26,,
39,46,19,24,37,47,24,39,24,39,18,15, & !27,,
37,43,18,23,35,44,23,37,23,36,17,14, & !28,,
34,40,16,22,34,42,22,34,22,34,15,14, & !29,,
32,38,15,21,32,39,21,32,21,31,14,13, & !30,,
30,35,15,20,31,37,20,30,20,29,13,12, & !31,,
29,33,14,19,29,34,19,29,19,27,13,12/ !32,,

HeightCurve15m = Hdom15(i,iSIS)
return
end