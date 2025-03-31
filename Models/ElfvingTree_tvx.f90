   	subroutine G3ElfvingBestTvx(IFIX,FIX,BEST,ART, GI)
! Out: Basal area growth per 5-years (m2/ha)
! In: see df under aa(.)
    USE G3_Global
    USE G3_NAMES
	INTEGER	:: IFIX(NFIX)
	REAL	:: FIX(NFIX),BEST(NBEST),ART(NART,MXSPECI)
    REAL	:: GI(MXSPECI)				

	integer	:: i
	real	:: aa(24),veg(18),a(24)

!		Estimate
data aa/			&			
		0.2702	,	&				
		-0.5819	,	&	!		lna	= ln(a), a är ålder enligt definition i texten
		8.1754	,	&	!		barrdga	= [barrträdsandel av grundytan (0-1)] / a
		-0.0233	,	&	!		tdveg	= (tallandel av grundytan) · veg
		-0.3163	,	&	!		bjdel2	= (björkandel av grundytan)²
		-10.5278,	&	!		bjdkyl	= bjdel·kyligt klimatläge '= exp[-0.01·(tsumma-300)], där tsumma '=    temperatursumma, dag-grader>+5 ºC '= 4835 - 57.6·latitud  0.9·altitud
		0.5416	,	&	!		lng	= ln(g), g är grundyta (m²/ha) för tillväxtmätta träd
		-0.00932,	&	!		g0	= total grundyta (m²/ha) vid tillväxtperiodens början
		0.1895	,	&	!		lnsn	= stamantal
		0.0622	,	&	!		veg	= kod enligt definition i texten
		-0.0252	,	&	!		torvveg	= veg om det finns torv på ytan, annars '= 0
		-0.0476	,	&	!		moist	= 1 om ytan är fuktig, annars '= 0
		-0.181	,	&	!		wet	= 1 om ytan är blöt, annars '= 0
		0.0113	,	&	!		sis	= ståndortsindex enlig ståndortsbonitering för bonitetsvisande trädslag (m),   medeltal av oberoende bestämningar vid I1 och I2
		0.0533	,	&	!		dikat	= 1 om dike finns inom 25 m från ytcentrum, annars '= 0
		0.3091	,	&	!		 =0 dp beräkning sker separat i GODEFFfertris	= värde mellan 0.2-1 på yta med fs-kod>12 som kvävegödslats inom 8 år före I2, med värdet beroende på tidpunkt för gödslingen, annars '= 0
		0.0755	,	&	!		kant	= 1 om beståndskant mot öppen mark finns inom 20 m från ytcentrum, annars '= 0
		0.0621	,	&	!		delad	= 1 om ytan är delad, annars '= 0
		0.1469	,	&	!		hu0t10	= 1 om huggning utförts inom 15 år före I2, annars '= 0
		0.0624	,	&	!		hu10t30	= 1 om huggning utförts 15-35 år för I2, annars '= 0
		0.1266	,	&	!		lngrel	= ln(g0/G), där G är grundyta i omgivande bestånd enligt relaskopmätning
		-0.061	,	&	!		a3	= 1 om ytan utlagts 1983, annars '= 0
		-0.0432	,	&	!		a4	= 1 om ytan utlagts 1984, annars '= 0
		0.0434/				!	a6	= 1 om ytan utlagts 1986, annars '= 0

Data veg/		&	!	Innebörd
		4.	,	&	!	H-ört u ris
		2.5	,	&	!	H-ört m blå
		2.	,	&	!	H-ört m ling
		3.	,	&	!	L-ört u ris
		2.5	,	&	!	L-ört m blå
		2.	,	&	!	L-ört m ling
		3.	,	&	!	Utan fs
		2.5	,	&	!	Breda gräs
		1.5	,	&	!	Smala gräs
		-3.	,	&	!	Högstarr
		-3.	,	&	!	Lågstarr
		1.	,	&	!	Fräken
		0.	,	&	!	Blåbär
		-0.5,	&		!	Lingon
		-3.	,	&	!	Kråkb/ljung
		-5.	,	&	!	Fattigris
		-0.5,	&		!	Lavrik
		-1.	/		!	Lav

GI(:)=0.
Gtot = sum(ART(Gs,:))
do i=1,MXSPECI
if(ART(Gs,i) > 0 )then
	a(1)	= 1. + log(0.67)				! Growth level correction 2021-02-15
    a(2)	= log(BEST(TotAge))
    a(3)	= ( BEST(SpPin)+BEST(SpSpr) )/BEST(TotAge)
    a(4)	=  BEST(SpPin)*veg(IFIX(Ftype))
    a(5)	= (ART(Gs,Birch)/Gtot)**2
    a(6)	= (ART(Gs,Birch)/Gtot)*exp(-0.01*(FIX(Tsum)-300.))
    a(7)	= log(Gtot)
    a(8)	= Gtot
    stlog 	= BEST(N)/(1.+max(0.,log(BEST(N)/500)))
    a(9)	= log(stlog)
    a(10)	= veg(IFIX(Ftype))
    a(11)	= 0.
    if( IFIX(Peat) == 1 ) a(11)	= veg(IFIX(Ftype))
    a(12)	= 0.
    if( IFIX(Moist) == 4 ) a(12) = 1.
    a(13)	= 0.
    if( IFIX(Moist) == 5 ) a(13) = 1.
    a(14)	= FIX(SI)
    a(15)	= IFIX(Ditch)
    a(16)	= 0.
    a(17)	= 0.
    a(18)	= 0.
    a(19)	= 0.
    a(20)	= 0.
    if( BEST(TimeTh) <= 15. ) then
      a(19)	= 1.
	elseif ( BEST(TimeTh) <= 35) then
      a(20)	= 1.
    endif
    a(21)	= 0.
    a(22)	= 0.
    a(23)	= 0.
    a(24)	= 0.

	Dd = sqrt( exp( sum(aa(:)*a(:)) ) )
	GI(i) = GI(i) + ART(Ns,i)*Dd*Dd/12732.4

endif
enddo

    return
    end

subroutine G3ElfvingTree(IFIX,FIX,BEST,ART, GI)
! Computes the basal area growth of species --> GI(.) (m2/ha and 5 years)
    USE G3_Global
    USE G3_NAMES
	INTEGER	:: IFIX(NFIX)
	REAL	:: FIX(NFIX),BEST(NBEST),ART(NART,MXSPECI)
    REAL	:: GI(MXSPECI)
!    
    INTEGER, PARAMETER	:: NoDiaCl=10
    INTEGER	:: i,j
    REAL	:: aa(10,31),a(31),NoInDiaClass(NoDiaCl,MXSPECI),DiaClassShare(NoDiaCl),	&
    			Ds(MXSPECI),Dt(NoDiaCl,MXSPECI),bal(NoDiaCl,MXSPECI),Dd

data aa/	&
! Pine	Spruce	Birch	Aspen	Oak	Beech	SouthBrl	Contorta	OtherBrl	Larch	Variabel
3.4176,    3.436,    5.9648,    0.9945,    1.7005,    1.9047,    2.3316,    2.1108,    2.1108,    2.1108,       &     ! 1 Konstant
1.0149,    1.5163,    1.2217,    1.9071,    2.5823,    1.3115,    0.825,    0.9418,    0.9418,    0.9418,       &     ! 2 ln (diam+ 1.0)
0,    -0.152,    0,    -0.3313,    -0.3758,    0,    0,    0,    0,    0,       &     ! 3 diam*10-1
-0.3902,    -0.4024,    -0.3998,    -0.304,    -0.2079,    -0.264,    -0.2877,    -0.2599,    -0.2599,    -0.2599,       &     ! 4 bal / (diam + 1.0) 
0,    0.4702,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 5 bal/(diam + 1.0) *((Mditot-Mgtot)/Mditot)3
-0.773,    -0.7789,    -0.9226,    -0.4058,    -0.4478,    0,    0,    -0.3026,    -0.3026,    -0.3026,       &     ! 6 ln (a13 + 20)
0.2218,    0.4034,    0.4772,    0,    0,    0,    0,    0,    0,    0,       &     ! 7 ost
0.1843,    0,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 8 Mditot*10-1
0,    0.1914,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 9 (Mditot)2*10-3
-0.3145,    -0.2342,    -0.209,    -0.1981,    -0.5348,    -0.5056,    -0.401,    -0.228,    -0.228,    -0.228,       &     ! 10 ln(Gry + 3.0)
0,    0.1625,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 11 bal/(diam + 1.0) *((Gry- Gry(gran))/Gry)
0.1391,    0.1754,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 12 ((Gry- Gry(trsl))/Gry)2
0,    0,    -0.5821,    -0.5967,    -0.9304,    -0.6001,    0,    0,    0,    0,       &     ! 13 (Gry(trsl)/Gry) 0.5
-0.0844,    -0.3264,    0,    0,    0,    -0.4615,    -0.3809,    0,    0,    0,       &     ! 14 Gotland
0,    -0.6923,    -0.5386,    0.4408,    0,    0,    0,    0,    0,    0,       &     ! 15 Ts*10-3
0.1178,    0.2568,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 16 (Ts*10-3)2
0,    0,    -0.4505,    0,    0,    0,    0,    0,    0,    0,       &     ! 17 1 / (Ts*10-3 - 0.3)
0,    0,    0.8801,    0,    0,    0,    0,    0,    0,    0,       &     ! 18 1 / ( Ak + 3.0)
0,    0,    0,    0,    -0.1906,    0,    0,    0,    0,    0,       &     ! 19 Lat-50
0,    0,    0,    0,    0,    0.3833,    0,    0,    0,    0,       &     ! 20 Alt*10-2
0,    0,    0,    0,    0,    -0.1938,    0,    0,    0,    0,       &     ! 21 (Alt*10-2)2
1.089,    0.2903,    0,    0,    0.3055,    0,    0,    0.2595,    0.2595,    0.2595,       &     ! 22 SIS *10-1
-0.2164,    0,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 23 SIS2 *10-2
0.1011,    0.1965,    0.3439,    0.4759,    0,    0.2635,    0,    0,    0,    0,       &     ! 24 Rich
0,    0,    0,    0,    0,    0,    0.9397,    0.4392,    0.4392,    0.4392,       &     ! 25 Herb
0.279,    0.4034,    0.3844,    0,    0,    0,    0,    0,    0,    0,       &     ! 26 Fertris
0.1245,    0.1309,    0.1814,    0.2143,    0.22,    0.1034,    0.241,    0.1561,    0.1561,    0.1561,       &     ! 27 Hu0t10
0.0451,    0,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 28 Hu11t25
0.0487,    0.0561,    0,    0,    0.2009,    0,    0,    0,    0,    0,       &     ! 29 Delad
0.1368,    0.1126,    0.2258,    0,    0,    0.3551,    0,    0,    0,    0,       &     ! 30 Kanteff
0.0842,    0.077,    0.1321,    0.2427,    0.2669,    0.1897,    0.4676,    0,    0,    0/     ! 31 ln(Gry /Gryf)

! Compute diameters and no. of stems in diameter classes, basal areas
Ds = 0.; Dt=0.; NoInDiaClass = 0.
do j=1,NoDiaCl
  	DiaClassShare(j) = DiaGreater(best(TotAge),20.,(-0.25 + j*0.25)*20.)
enddo
forall( j=1:NoDiaCl-1)DiaClassShare(j) = DiaClassShare(j) - DiaClassShare(j+1)
do i=1,MXSPECI
	if(art(Gs,i) > 0.1 )then
		Ds(i) = G3DIA(art(Gs,i),art(Ns,i))
       	forall( j=1:NoDiaCl ) Dt(j,i) = ((-0.25 + j*0.25)+0.125)*Ds(i)
    	forall( j=1:NoDiaCl ) NoInDiaClass(j,i) = DiaClassShare(j) * art(Ns,i)
    endif
enddo
bal(:,:) = 0.
do i=1,MXSPECI
	if(art(Gs,i) > 0.1 )then
  		do j=1,NoDiaCl
			bal(j,i)=sum( Dt(:,:)*Dt(:,:)*NoInDiaClass(:,:), Dt(:,:)>Dt(j,i) ) /12732.4
        enddo
    endif
enddo

GI(:)=0.
do i=1,MXSPECI
do j=1,NoDiaCl
	if(art(Gs,i) > 0.1 )then
		a(1)	= 1.	! Konstant
		a(2)	= log(Dt(j,i)+1.) !ln (diam+ 1.0)
		a(3)	= Dt(j,i)*0.1	! diam*10-1
		a(4)	= bal(j,i)/(Dt(j,i)+1.)	! bal / (diam + 1.0) 
		a(5)	= 0	! bal/(diam + 1.0) *((Mditot-Mgtot)/Mditot)3
		a(6)	= log(art(BHAs,i) + 20.)	! ln (a13 + 20)
		a(7)	= 0.! ost
		a(8)	= DB*0.1	! Mditot*10-1
		a(9)	= DB*best(D)*1.e-3	! (Mditot)2*10-3
		a(10)	= log(best(G)+3.)	! ln(Gry + 3.0)
		a(11)	= bal(j,i)/(Dt(j,i)+1.)*((best(G)-art(Gs,Spruce))/best(G))	! bal/(diam + 1.0) *((Gry- Gry(gran))/Gry)
		a(12)	= ((best(G)-art(Gs,i))/best(G))*((best(G)-art(Gs,i))/best(G))	! ((Gry- Gry(trsl))/Gry)2
		a(13)	= SQRT(art(Gs,i)/best(G))	! (Gry(trsl)/Gry) 0.5
		a(14)	= 0.	! Gotland
		a(15)	= FIX(Tsum)*1.e-3	! Ts*10-3
		a(16)	= a(15)**2	! (Ts*10-3)2
		a(17)	= 1./(a(15) - 0.3)	! 1 / (Ts*10-3 - 0.3)
		a(18)	= 1./(min(30.,FIX(ASL)*30./500.)+3.)	! 1 / ( Ak + 3.0)
		a(19)	= FIX(Lat)-50.	! Lat-50
		a(20)	= FIX(ASL)*1.e-2	! Alt*10-2
		a(21)	= a(20)**2	! (Alt*10-2)2
		a(22)	= FIX(SI)*0.1	! SIS *10-1
		a(23)	= FIX(SI)*FIX(SI)*1.e-2	! SIS2 *10-2
		a(24)	= 0.	! Rich
		if(IFIX(Ftype) < 10 .or. IFIX(Ftype) == 12) a(24) = 1.	! Rich
		a(25)	= 0.	! Herb
		if(IFIX(Ftype) < 7) a(25)	= 1.	! Herb
		a(26)	= 0.	! Fertris
		a(27)	= 0.	! Hu0t10
		a(28)	= 0.	! Hu11t25
		if(best(TimeTh) <= 10.1)then
		  a(27)	= 1.	! Hu0t10
		elseif(best(TimeTh) <= 25.1)then
		  a(28)	= 1.	! Hu11t25
		endif  
		a(29)	= 0.	! Delad
		a(30)	= 0.
        	! Kanteff
		a(31)	= 0.	! ln(Gry /Gryf)
		Dd = sqrt( exp( sum(aa(i,:)*a(:)) ) )
		GI(i) = GI(i) + NoInDiaClass(j,i)*Dd*Dd/12732.4
endif
enddo;enddo

return
end

REAL FUNCTION DiaGreater(Age,Ds,Dt)
! DiaGreater = the share of trees for a tree with dia Dt that is larger than stand diameter Ds
! Ds 	= limit diameter
! Dt	= diameter of the tree
! Age	= average age of the stand
	real	:: Age,Ds,Dt

	DiaGreater = 1. - ( 1-EXP(-1*(Dt/(0.65+1.053*Ds))**(4.73 -  0.9987*LOG(16.59088) - 0.0217*LOG(Age+1))) ) 

return
end

subroutine G3ElfvingMeanTree(IFIX,FIX,TimeFrTh,ART, GI)
! Computes the basal area growth of species --> GI(.) (m2/ha and 5 years)
    USE G3_Global
    USE G3_NAMES
	INTEGER	:: IFIX(NFIX)
	REAL	:: FIX(NFIX),TimeFrTh,ART(NART,MXSPECI)
    REAL	:: GI(MXSPECI)
!    
    INTEGER	:: i,Sp2Sp(MXSPECI)
    REAL	:: aa(10,31),a(31),Dt,Dd,NB,GB,DB

    ! Recode from common species order to species order of data here
    DATA Sp2Sp/ 1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	4,	4 /

data aa/	&
! Pine	Spruce	Birch	Aspen	Oak	Beech	SouthBrl	Contorta	OtherBrl	Larch	Variabel
3.4176,    3.436,    5.9648,    0.9945,    1.7005,    1.9047,    2.3316,    2.1108,    2.1108,    2.1108,       &     ! 1 Konstant
1.0149,    1.5163,    1.2217,    1.9071,    2.5823,    1.3115,    0.825,    0.9418,    0.9418,    0.9418,       &     ! 2 ln (diam+ 1.0)
0,    -0.152,    0,    -0.3313,    -0.3758,    0,    0,    0,    0,    0,       &     ! 3 diam*10-1
-0.3902,    -0.4024,    -0.3998,    -0.304,    -0.2079,    -0.264,    -0.2877,    -0.2599,    -0.2599,    -0.2599,       &     ! 4 bal / (diam + 1.0) 
0,    0.4702,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 5 bal/(diam + 1.0) *((Mditot-Mgtot)/Mditot)3
-0.773,    -0.7789,    -0.9226,    -0.4058,    -0.4478,    0,    0,    -0.3026,    -0.3026,    -0.3026,       &     ! 6 ln (a13 + 20)
0.2218,    0.4034,    0.4772,    0,    0,    0,    0,    0,    0,    0,       &     ! 7 ost
0.1843,    0,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 8 Mditot*10-1
0,    0.1914,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 9 (Mditot)2*10-3
-0.3145,    -0.2342,    -0.209,    -0.1981,    -0.5348,    -0.5056,    -0.401,    -0.228,    -0.228,    -0.228,       &     ! 10 ln(Gry + 3.0)
0,    0.1625,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 11 bal/(diam + 1.0) *((Gry- Gry(gran))/Gry)
0.1391,    0.1754,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 12 ((Gry- Gry(trsl))/Gry)2
0,    0,    -0.5821,    -0.5967,    -0.9304,    -0.6001,    0,    0,    0,    0,       &     ! 13 (Gry(trsl)/Gry) 0.5
-0.0844,    -0.3264,    0,    0,    0,    -0.4615,    -0.3809,    0,    0,    0,       &     ! 14 Gotland
0,    -0.6923,    -0.5386,    0.4408,    0,    0,    0,    0,    0,    0,       &     ! 15 Ts*10-3
0.1178,    0.2568,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 16 (Ts*10-3)2
0,    0,    -0.4505,    0,    0,    0,    0,    0,    0,    0,       &     ! 17 1 / (Ts*10-3 - 0.3)
0,    0,    0.8801,    0,    0,    0,    0,    0,    0,    0,       &     ! 18 1 / ( Ak + 3.0)
0,    0,    0,    0,    -0.1906,    0,    0,    0,    0,    0,       &     ! 19 Lat-50
0,    0,    0,    0,    0,    0.3833,    0,    0,    0,    0,       &     ! 20 Alt*10-2
0,    0,    0,    0,    0,    -0.1938,    0,    0,    0,    0,       &     ! 21 (Alt*10-2)2
1.089,    0.2903,    0,    0,    0.3055,    0,    0,    0.2595,    0.2595,    0.2595,       &     ! 22 SIS *10-1
-0.2164,    0,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 23 SIS2 *10-2
0.1011,    0.1965,    0.3439,    0.4759,    0,    0.2635,    0,    0,    0,    0,       &     ! 24 Rich
0,    0,    0,    0,    0,    0,    0.9397,    0.4392,    0.4392,    0.4392,       &     ! 25 Herb
0.279,    0.4034,    0.3844,    0,    0,    0,    0,    0,    0,    0,       &     ! 26 Fertris
0.1245,    0.1309,    0.1814,    0.2143,    0.22,    0.1034,    0.241,    0.1561,    0.1561,    0.1561,       &     ! 27 Hu0t10
0.0451,    0,    0,    0,    0,    0,    0,    0,    0,    0,       &     ! 28 Hu11t25
0.0487,    0.0561,    0,    0,    0.2009,    0,    0,    0,    0,    0,       &     ! 29 Delad
0.1368,    0.1126,    0.2258,    0,    0,    0.3551,    0,    0,    0,    0,       &     ! 30 Kanteff
0.0842,    0.077,    0.1321,    0.2427,    0.2669,    0.1897,    0.4676,    0,    0,    0/     ! 31 ln(Gry /Gryf)

GI(:)=0.

NB = sum(art(Ns,:))
GB = sum(art(Gs,:))
DB = G3DIA(GB,NB)

do i=1,MXSPECI
	if(art(Gs,i) > 0.1 )then
		Dt = G3DIA(art(Gs,i),art(Ns,i))
		a(1)	= 1.	! Konstant
		a(2)	= log(Dt+1.) !ln (diam+ 1.0)
		a(3)	= Dt*0.1	! diam*10-1
		a(4)	= ART(Gs,i)/(Dt+1.)	! bal / (diam + 1.0) 
		a(5)	= 0	! bal/(diam + 1.0) *((Mditot-Mgtot)/Mditot)3
		a(6)	= log(art(BHAs,i) + 20.)	! ln (a13 + 20)
		a(7)	= 0.! ost
		a(8)	= DB*0.1	! Mditot*10-1
		a(9)	= DB*DB*1.e-3	! (Mditot)2*10-3
		a(10)	= log(GB+3.)	! ln(Gry + 3.0)
		a(11)	= ART(Gs,i)/(Dt+1.)*((GB-art(Gs,Spruce))/GB)	! bal/(diam + 1.0) *((Gry- Gry(gran))/Gry)
		a(12)	= ((GB-art(Gs,i))/GB)*((GB-art(Gs,i))/GB)	! ((Gry- Gry(trsl))/Gry)2
		a(13)	= SQRT(art(Gs,i)/GB)	! (Gry(trsl)/Gry) 0.5
		a(14)	= 0.	! Gotland
		a(15)	= FIX(Tsum)*1.e-3	! Ts*10-3
		a(16)	= a(15)**2	! (Ts*10-3)2
		a(17)	= 1./(a(15) - 0.3)	! 1 / (Ts*10-3 - 0.3)
		a(18)	= 1./(min(30.,FIX(ASL)*30./500.)+3.)	! 1 / ( Ak + 3.0)
		a(19)	= FIX(Lat)-50.	! Lat-50
		a(20)	= FIX(ASL)*1.e-2	! Alt*10-2
		a(21)	= a(20)**2	! (Alt*10-2)2
		a(22)	= FIX(SI)*0.1	! SIS *10-1
		a(23)	= FIX(SI)*FIX(SI)*1.e-2	! SIS2 *10-2
		a(24)	= 0.	! Rich
		if(IFIX(Ftype) < 10 .or. IFIX(Ftype) == 12) a(24) = 1.	! Rich
		a(25)	= 0.	! Herb
		if(IFIX(Ftype) < 7) a(25)	= 1.	! Herb
		a(26)	= 0.	! Fertris
		a(27)	= 0.	! Hu0t10
		a(28)	= 0.	! Hu11t25
		if(TimeFrTh <= 10.1)then
		  a(27)	= 1.	! Hu0t10
		elseif(TimeFrTh <= 25.1)then
		  a(28)	= 1.	! Hu11t25
		endif  
		a(29)	= 0.	! Delad
		a(30)	= 0.
        	! Kanteff
		a(31)	= 0.	! ln(Gry /Gryf)

		Dd = sqrt( exp( sum(aa(Sp2Sp(i),:)*a(:)) ) )
		GI(i) = Art(Ns,i)*Dd*Dd/12732.4

	endif
enddo

return
end
