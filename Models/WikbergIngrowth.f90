subroutine WikbvergIngrowth(IFIX,FIX,Tage,ART, NI,GI)
! Computes the ingrowth in stems (NI) and basal area (GI) --> (stems and m2/ha and 5 years)
    USE G3_Global
    USE G3_NAMES
	INTEGER	:: IFIX(NFIX)
	REAL	:: FIX(NFIX),ART(NART,MXSPECI),Tage
    REAL	:: NI(MXSPECI),GI(MXSPECI)
!    
   INTEGER	:: i,its,Sp2Sp(MXSPECI)
   REAL	:: aa(10,34),a(34),bb(10,6),b(6),cc(10,11),c(11),dd(10,3),d3(3)
   REAL	:: lpi,pi,Nsmall,inp,Dia,GB

    ! Recode from common species order to species order of data here
    DATA Sp2Sp/ 1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	4,	4 /

data aa/	& ! Tabell 1a.  Sannolikhet för förekomst av små träd 1-39 mm
! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch
8.4197, -7.0172, 52.155, -1.0898, 33.976, -9.3382, -12.506, 0, -1.0898, -1.0898,    &    !  1 Konstant
-0.075, -0.032, -0.1469, -0.027, -0.021, -0.0231, 0, 0, -0.027, -0.027,    &    !  2    Gry
0, 0.3505, 0.9159, 0, 0, 0, 0, 0, 0, 0,    &    !  3 
0.0129, 0.0052, 0.0024, 0, -0.0073, -0.0118, 0, 0, 0, 0,    &    !  4 Bald
-1.732, 0, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  5 ln(Bald)
0, 60.0307, 47.101, 0, 0, 0, 0, 0, 0, 0,    &    !  6 1/(5+Bald)
-0.2388, 0.7613, 0, 0, 0, 0, 0.472, 0, 0, 0,    &    !  7 Ts*0.01
0, -0.0393, 0, 0, 0, 0.0285, 0, 0, 0, 0,    &    !  8 (Ts*0.01)2
-0.339, 0, 0, -0.3665, -0.8561, 0, 0, 0, -0.3665, -0.3665,    &    !  9 Alt*0.01
0, 0, -0.1945, 0, -0.059, 0, 0, 0, 0, 0,    &    !  10 Lat
0, 0, 0.0169, 0, 0, 0, 0, 0, 0, 0,    &    !  11 Lat2*0.01
0.4464, 0, 0.4916, 0, 0, 0, 0, 0, 0, 0,    &    !  12 Gran_små
1.5613, 0, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  13 Tall_10
-0.5922, 1.4845, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  14 Gran_10
0, 0.3715, 0.8687, 0, 0, 0, 0, 0, 0, 0,    &    !  15 Björk_10
0, 0, 0, 1.3327, 0, 0, 0, 0, 1.3327, 1.3327,    &    !  16 Ölöv1_10
0, 0, 0, 0, 1.1611, 0, 0, 0, 0, 0,    &    !  17 Ek_10
0, 0, 0, 0, 0, 4.0056, 0, 0, 0, 0,    &    !  18 Bok_10
0, 0, 0, 0, 0, 0, 3.3092, 0, 0, 0,    &    !  19 Ölöv2_10
0, 0, -0.6547, 0, 0, 0, 0, 0, 0, 0,    &    !  20 Clean
-0.0984, 0.2927, 0.5942, 0.2112, -0.4802, 0, 0, 0, 0.2112, 0.2112,    &    !  21 Moisture
0.8961, 0, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  22 Ppoor
0, 0, 0, -0.5936, 0, 0, 0, 0, -0.5936, -0.5936,    &    !  23 Shrubs
0, -0.2371, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  24 Spoor
-0.923, 0, 0, 0, 0, 0, 2.3856, 0, 0, 0,    &    !  25 Herb
0, 0, 0, 0.6112, 0, 0, 0, 0, 0.6112, 0.6112,    &    !  26 Herbob
0, 0.411, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  27 Hwd
0, -0.3078, -0.4979, 0, 0, 0, 0, 0, 0, 0,    &    !  28 Hwod
0, 0, -0.6733, 0, 0, 0.9387, 0, 0, 0, 0,    &    !  29 Noveg
0, 0, 0.2284, 0, 0, 0, 0, 0, 0, 0,    &    !  30 Brgrass
0, 0, 0, 0, 0, 0.6695, 0, 0, 0, 0,    &    !  31 Grass
0, 0, 0, -1.4621, 0, 0, 0, 0, -1.4621, -1.4621,    &    !  32 Carex
0, 0, -0.9416, -1.4304, 0, 0, 0, 0, -1.4304, -1.4304,    &    !  33 Lichen
0, 0, 0.1769, -0.1545, 0, 0, 0, 0, -0.1545, -0.1545 /        !  34 Peat

data bb/	& ! 1 Tabell 1b.  Antal små träd givet Förekomst 1-39 mm
! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch
0.3224, 2.2215, 7.7188, 0.6155, 8.3071, 6.6233, 8.4818, 0, 0.6155, 0.6155,    &    !  1 b0
0.5954, 0.4947, 0.4407, 0.4989, 0.3272, 0.0324, 0.1551, 0, 0.4989, 0.4989,    &    !  2 lpi
0.0635, 0.0127, 0, -0.0383, 0, 0, 0, 0, -0.0383, -0.0383,    &    !  3 Gry
0, 0, -0.0044, 0, -0.0121, -0.0141, 0, 0, 0, 0,    &    !  4 Bald
0.1424, 0, 0, 0.7363, 0, 0, 0, 0, 0.7363, 0.7363,    &    !  5 Ts*0.01
0, 0, 0, -0.0352, 0, 0, 0, 0, -0.0352, -0.0352/        !  6 (Ts*0.01)2

data cc/	& ! Tabell 1c.  Sannolikhet för inväxning >39 mm dbh
! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch
-2.227, -0.249, -3.5299, -3.0504, -3.1625, -2.7622, -3.07382, 0, -3.0504, -3.0504,    &    !  1 Konstant
-0.07, 0, -0.0275, 0, 0, 0, 0, 0, 0, 0,    &    !  2 Gry
0, -0.4293, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  3 Sqrt(Gry)
-0.00692, -0.00619, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  4 Bald
0.5333, 0.4782, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  5 Clthinn_05
-1.4684, 0, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  6 Gry(gran)/Gry
0.0489, 0.0156, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  7 SI
0, 0, 0.2405, 0, 0, 0, 0, 0, 0, 0,    &    !  8 Rich
0, 0, 0.3512, 0, 0, 0, 0, 0, 0, 0,    &    !  9 Mesic
0, -0.1713, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  10 Ln(Alt*0.01)
0.027, 0, 0, 0, 0, 0, 0, 0, 0, 0/        !  11 (Alt*0.01)2

data dd/	& ! Tabell 1d.  Medeldiameter för inväxta träd
! Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch
7.3132, 7.231, 7.4224, 48.9, 47.3, 43.6, 45.1, 0, 48.9, 48.9,    &    !  1 Konstant
5.138, 5.0592, 0, 0, 0, 0, 0, 0, 0, 0,    &    !  2 1/(5+Gry)
0.00838, 0.01031, 0.0179, 0, 0, 0, 0, 0, 0, 0/    !  3 Si

NI(:)=0.0; GI(:)=0.0; a(:)=0.0; b(:)=0.0; c(:)=0.0; d3(:)=0.0

GB = sum(ART(GS,:))

do its=1,MXSPECI
  i=Sp2Sp(its)
	if(art(Gs,i) > 0.1 .and. i/=Contorta )then
		a(1)		= 1.		! 1	Konstant
		a(2)		= GB		! 2	   Gry
		a(3)		= sqrt(GB)		! 3	sqrt(Gry)
		a(4)		= Tage		! 4	Bald
		a(5)		= log(Tage)		! 5	ln(Bald)
		a(6)		= 1./(5. + Tage)	! 6	1/(5+Bald)
		a(7)		= FIX(Tsum)*0.01		! 7	Ts*0.01
		a(8)		= (FIX(Tsum)*0.01)**2		! 8	(Ts*0.01)2 
		a(9)		= FIX(ASL)*0.01		! 9	Alt*0.01
		a(10)		= FIX(Lat)*10.		! 10	Lat
		a(11)		= (FIX(Lat)*10.)**2*0.01	! 11	Lat2*0.01
		a(12)		= 0.		! 12	Gran_små
		if( ART(GS,Pine)>0.1 )	a(13)		= 1.		! 13	Tall_10
		a(14)		= ART(GS,Spruce)/GB		! 14	Gran_10
		if( ART(GS,Birch)>0.1 )a(15)		= 1.		! 15	Björk_10
		if( ART(GS,Aspen)>0.1 .or. ART(GS,OtherBrl)>0.1 .or. ART(GS,Larch)>0.1 )	a(16)		= 1.		! 16	Ölöv1_10
		if( ART(GS,Oak)>0.1 )	a(17)		= 1.		! 17	Ek_10
		if( ART(GS,Beech)>0.1 )	a(18)		= 1.		! 18	Bok_10
		if( ART(GS,SouthBrl)>0.1 )	a(19)		= 1.		! 19	Ölöv2_10
		a(20)		= 0.		! 20	Clean
		a(21)		= min(IFIX(Moist),4)		! 21	Moisture
		if( ( IFIX(Ftype)>= 14 .and. IFIX(Ftype)<= 18 ) .or. IFIX(Ftype) == 11 )	a(22)	= 1.		! 22	Ppoor
		if( IFIX(Ftype)>= 13 .and. IFIX(Ftype)<= 18 )	a(23)		= 1.		! 23	Shrubs
		if( IFIX(Ftype)>= 14 .and. IFIX(Ftype)<= 18 )	a(24)		= 1.		! 24	Spoor
		if( IFIX(Ftype)<7 )	a(25)		= 1.		! 25	Herb
		if( IFIX(Ftype)<5 )	a(26)		= 1.		! 26	Herbob
		if( IFIX(Ftype)>=2 .and. IFIX(Ftype)<=6 .and. IFIX(Ftype)/=4 )	a(27)		= 1.		! 27	Hwd
		if( IFIX(Ftype)==1 .or. IFIX(Ftype)==4 )	a(28)		= 1.		! 28	Hwod
		if( IFIX(Ftype)==7 )	a(29)		= 1.		! 29	Noveg
		if( IFIX(Ftype)==8 )	a(30)		= 1.		! 30	Brgrass
		if( IFIX(Ftype)==9 )	a(31)		= 1.		! 31	Grass
		if( IFIX(Ftype)==10 )	a(32)		= 1.		! 32	Carex
		if( IFIX(Ftype)==17 .or. IFIX(Ftype)==18 )	a(33)		= 1.		! 33	Lichen
		if( IFIX(Peat)==1 )	a(34)		= 1.		! 34	Peat

		lpi = dot_product(aa(Sp2Sp(i),:),a)
        pi  = 1./(1.+exp(-lpi))		

		b(1)		= 1.		! 1	b0
		b(2)		= lpi		! 2	lpi
		b(3)		= GB		! 3	Gry
		b(4)		= Tage		! 4	Bald
		b(5)		= FIX(Tsum)*0.01		! 5	Ts*0.01
		b(6)		= (FIX(Tsum)*0.01)**2		! 6	(Ts*0.01)2
		Nsmall = dot_product(bb(Sp2Sp(i),2:6),b(2:6))
		Nsmall = 1. + bb(i,1)*exp(Nsmall)
        Nsmall = Nsmall*10000./(3.14*5.*5.)

		c(1)		= 1.		! 1	Konstant
		c(2)		= GB		! 2	Gry
		c(3)		= sqrt(GB)		! 3	Sqrt(Gry)
		c(4)		= Tage		! 4	Bald
		c(5)		= 0.		! 5	Clthinn_05
		c(6)		= ART(GS,Spruce)/GB		! 6	Gry(gran)/Gry
		c(7)		= FIX(SI)		! 7	SI
		c(8)		= 0.		! 8	Rich
		if( IFIX(Ftype) < 9 ) c(8)		= 1.
		c(9)		= 0.		! 9	Mesic
		if( IFIX(Moist) == 3 ) c(9)		= 1.
		c(10)		= log( (FIX(ASL)+1.)*0.01 )		! 10	Ln(Alt*0.01)
		c(11)		= (FIX(ASL)*0.01)**2		! 11	(Alt*0.01)2

        inp = dot_product(cc(Sp2Sp(i),:),c)
        inp = 1./(1.+exp(-inp))

        NI(i) = ART(Gs,i)/GB*pi*Nsmall*inp
    endif

	if( NI(i) > 0. )then
		d3(1)		= 1.		! 1	Konstant
		d3(2)		= 1./(5.+GB)		! 2	1/(5+Gry)
		d3(3)		= FIX(SI)		! 3	Si
    	Dia = log( dot_product(dd(Sp2Sp(i),:),d3) )
		GI(I) = G3YTA(Dia,NI(i))
   endif

enddo;

return
end
