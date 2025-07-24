	REAL FUNCTION G3VOL(I,ART,IFIX,FIX) result(GxVOL)
!***********************************************************************
!  Static volume assessment for species no. I
!***********************************************************************
 	USE G3_Global
    USE G3_NAMES
	USE G3_interfaces
    INTEGER :: IFIX(NFIX)
    REAL    :: FIX(NFIX)
    REAL    :: ART(NART,MXSPECI)
    INTEGER,PARAMETER	:: mn=100    ! max antal trd
!
!      
    real ::     fh(MXSPECI),hd
	DATA        hd/10./                ! ™vre h”jd f”r inv„xning
    DATA		fh/4.64,4.59,4.21,3.89,4.09,3.96,4.28,4.62,4.16,4.09,3.89,3.89/	! Form height from NFI 14-18, 7-10 m
	INTEGER	:: i,its,DomSpec,roj
    REAL	:: VolBrandel,Ntot,DomH,Dia,Gy


    INTEGER :: Sp2Br(MXSPECI)
!        1=TALL,2=GRAN,3=BJ\RK,4=\VR.L\V,5=BOK,6=EK (KOD 5,6 [NDR 8509)
    ! Recode from common species order to Brandel order
    DATA Sp2Br/ 1,	2,	3,	4,	6,	5,	4,	1,	4,	4,	4,	4 /

!        its=Sp2Br(i)
!        if(ART(Hs,i) >= hd)then
!            G3VOL = VolBrandel('pb',FIX(Lat),FIX(ASL),its, &
!                G3DIA(art(Gs,i),art(Ns,i)),ART(Hs,i)) * art(Ns,i) / 1000. 
!        else
!            G3VOL = art(Gs,i) * fh(i)
!        endif


   	its=Sp2Br(i)
    Dia=G3DIA(art(Gs,i),art(Ns,i))
!    if(ART(Hs,i) >= hd .and. art(Gs,i) > 0.)then
    if(Dia > 5. .and. ART(Hs,i) > 2.)then
      	Hin=ART(Hs,i)
       	GxVOL = VolBrandel('pb',FIX(Lat),FIX(ASL),its,Dia,Hin) * art(Ns,i) / 1000. 
 	elseif(ART(Hs,i) > 0.)then
		Gy= G3YTA(Dia,ART(Ns,i))
		GxVOL= Gy*fh(i)
    else
      	GxVOL=0.
    endif

    RETURN
    END

	REAL FUNCTION G3HOJD(I,ART,IFIX,FIX) result(GxHOJD)
!***********************************************************************
!  Height at total at age A1+5 based on existing height at age A1
!***********************************************************************
 	USE G3_Global
    USE G3_NAMES
	USE G3_interfaces
    INTEGER :: IFIX(NFIX),I
    REAL    :: FIX(NFIX),ART(NART,MXSPECI)
    REAL    :: HeurekaHojd,A1,A2,H1,T13
!      
    A1=ART(BHAs,i)+T13(i,FIX(SI))
    A2=A1+5.
    H1=ART(Hs,i)
    GxHOJD=HeurekaHojd(i,A1,H1)

    RETURN
    END

    real function dfh(art) result(dfhx)
  	USE G3_Global
    USE G3_NAMES
	USE G3_interfaces
    real    :: art(nart)
    
       real    :: fhf(5)
    DATA        fhf/1.183722043,-0.040572791,-0.00030491,0.001901613,0.443051145/

            dia=G3DIA(art(Gs),art(Ns))
            dfhx=fhf(1)+dia*fhf(2)+art(Vs)*fhf(3)+art(BHAs)*fhf(4)+art(Hs)*fhf(5)   ! Regression på NFI: 2018-2022
            art(Gs)=art(Vs)/dfhx
            dia=G3DIA(art(Gs),art(Ns))
           dfhx=fhf(1)+dia*fhf(2)+art(Vs)*fhf(3)+art(BHAs)*fhf(4)+art(Hs)*fhf(5) ! Regression på NFI: 2018-2022
    return
    end