REAL FUNCTION BREEDING(FF,species) result(BREEDINGx)
!---------------------------------------------------------------------------------
! Breeding factor - to be multiplied with BA growth
! Assume current practical breeding effect 10%
! Assume increase of breeding effect 0.5%/y x 0.75 (pollen from outside orchard)
! Data from Produktionshöjande åtgärder
! RAPPORT 2018/1; Produktionshöjande åtgärder; Rapport från samverkansprocess skogsproduktion
! https://www.skogsstyrelsen.se/globalassets/om-oss/rapporter/rapporter-2018/rapport-20181-produktionshojande-atgarder.pdf
!
! FF = years since final felling
!---------------------------------------------------------------------------------
    INTEGER ::  species
    REAL ::     FF
!
	if( species <= 2)then
		BREEDINGx = 0.1+0.005*0.75*FF
    elseif( species == 8)then                   ! Contorta
		BREEDINGx = ( 0.1+0.005*0.75*FF ) * 2.
	else
    	BREEDINGx =  0
	endif

    RETURN
    END
    

REAL FUNCTION BREEDING_Ha(FF,species) result(BREEDING_Hax)
!---------------------------------------------------------------------------------
! Version A: Breeding factor from default values in Heureka - to be multiplied with BA growth
!
! FF = years since final felling
!---------------------------------------------------------------------------------
    INTEGER ::  species
    REAL ::     FF

	INTEGER	::i
	REAL	:: breedingfactor(300)
    DATA breedingfactor/			&
0.03762,0.0864,0,0,0,0,0,0,0,0,	&
0.0423225,0.0972,0,0,0,0,0,0,0,0,	&
0.047025,0.138,0,0,0,0,0,0,0,0,	&
0.053295,0.1564,0,0,0,0,0,0,0,0,	&
0.059565,0.1805,0,0,0,0,0,0,0,0,	&
0.065835,0.1995,0,0,0,0,0,0,0,0,	&
0.072105,0.2185,0,0,0,0,0,0,0,0,	&
0.078375,0.2375,0,0,0,0,0,0,0,0,	&
0.084645,0.2565,0,0,0,0,0,0,0,0,	&
0.090915,0.2755,0,0,0,0,0,0,0,0,	&
0.097185,0.2945,0,0,0,0,0,0,0,0,	&
0.103455,0.3135,0,0,0,0,0,0,0,0,	&
0.109725,0.3325,0,0,0,0,0,0,0,0,	&
0.115995,0.3515,0,0,0,0,0,0,0,0,	&
0.122265,0.3705,0,0,0,0,0,0,0,0,	&
0.1269675,0.38475,0,0,0,0,0,0,0,0,	&
0.13167,0.399,0,0,0,0,0,0,0,0,	&
0.134805,0.4085,0,0,0,0,0,0,0,0,	&
0.13794,0.418,0,0,0,0,0,0,0,0,	&
0.13794,0.418,0,0,0,0,0,0,0,0,	&
0.13794,0.418,0,0,0,0,0,0,0,0,	&
0.13794,0.418,0,0,0,0,0,0,0,0,	&
0.13794,0.418,0,0,0,0,0,0,0,0,	&
0.13794,0.418,0,0,0,0,0,0,0,0,	&
0.13794,0.418,0,0,0,0,0,0,0,0,	&
0.13794,0.418,0,0,0,0,0,0,0,0,	&
0.13794,0.418,0,0,0,0,0,0,0,0,	&
0.13794,0.418,0,0,0,0,0,0,0,0,	&
0.13794,0.418,0,0,0,0,0,0,0,0,	&
0.13794,0.418,0,0,0,0,0,0,0,0/

    i=(FF/5.-1)*10+species
	BREEDING_Hax = breedingfactor(i)

    RETURN
    END
    