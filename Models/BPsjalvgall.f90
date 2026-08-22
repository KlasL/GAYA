      REAL FUNCTION BPSJGALL(i,gryta,stot,akl,ireg) 
!-----------------------------------------------------------------------
! Bengtsson, G. 1978. Beräkning av den naturliga avgången i 
! avverkningsberäkningarna för 1973 års skogsutrednings slutbetänkande. 
! I: Skog för framtid, SOU 1978:7, bilaga 6.
!
!  (in) i       = species	
!      	gryta   = grundyta (m2/ha)
!       stot    = stamantal (/ha)
!       akl     = total†lder
!       ireg    = reion (1=  ; 2=  ; 3=
!  (ut) avg†ng (5 †r) andel av grundyta f”r tr„dslag i (dec.)
!       antag relativ dia hos tr„d som avi(dec. )= 1
!-----------------------------------------------------------------------
	USE G3_Global
    USE G3_NAMES
    
!-- formella parameterar
      INTEGER*4 i,ireg
      REAL*4    akl,gryta,stot
!-- lokala deklarationer
      INTEGER*4 j
      REAL*4    alkl,tot,tots,a1,a2,agSpec
!                  1    2     3      4        5    6    7  
! Species Hugin: Pine,Spruce,Birch,Other bl,Beech,Oak,Contorta
!                 1      2     3     4    5    6      7       8          9     10     11     12
! Species here: Pine,Spruce,Birch,Aspen,Oak,Beech,SouthBrl,Contorta,OtherBrl,Larch,HybAsp,Poppel
	INTEGER	:: Hu2Sp(MXSPECI)
    DATA Hu2Sp/1,2,3,4,6,5,4,1,4,1,4,4/
    DATA agSpec/0.01/ ! Provisional natural mortality 5 years for special species

! Special species
	if(i == HybAsp .or. i == Poppel)then
    	BPSJGALL=agSpec
        return
    endif

!-- †ldersklass
      if(akl.lt.100)then
         ALKL=akl/10.+1.
      else
         ALKL=min((akl-100)/20*2+12.,17.)
      endif

!-- tr„dslagsvis ber„kning
	j = Hu2Sp(i)
         TOT=GRYTA
         TOTS=STOT
         IF(IREG.lt.3)then
            IF(J.eq.1)then
!              TALL NORRA- OCH MELLAN-SVERIGE
               IF(TOTS.GT.2700) TOTS=2700
               A1=.03143-.006877*TOT+.0002056*TOT*TOT+.00002684*TOTS-.5092E-08*TOTS*TOTS
               A2=.14
            elseif(J.eq.2)then
!              GRAN NORRA- OCH MELLAN-SVERIGE
               A1=-.002748+.00004493*TOT+.00002515*TOT*TOT
               A2=-.000236+.0250275*ALKL
            elseif(j.eq.3)then
!              BJ\RK NORRA- OCH MELLAN-SVERIGE
               A1=-.02513+.005484*TOT
               A2=.78
            else
!              \VRIGT NORRA- OCH MELLAN-SVERIGE
               A1=-.007277-.002456*TOT+.0001923*TOT*TOT
               A2=.35
            endif
         else
            if(J.eq.1)then
!              TALL S\DRA SVERIGE
               IF(TOTS.GE.4000) TOTS=4000
               A1=-.06766-.001283*TOT+.00007748*TOT*TOT+.0001441*TOTS-.1839E-07*TOTS*TOTS
               A2=.38
            elseif(J.eq.2)then
!              GRAN S\DRA SVERIGE
               IF(TOTS.GT.2800)TOTS=2800
               A1=.01235-.002749*TOT+.00008214*TOT*TOT+.00002457*TOTS-.4498E-08*TOTS*TOTS
               A2=.36
            else
!              BJ\RK+\VRIGT S\DRA SVERIGE
               A1=.04
               A2=.46
            endif
         endif

!-- Ta bort tr„ngselavg†ng f”r att f† ”verensst„mmelse med HUGIN
      A1=0.0

!     BER[KNING AV NETTOGRUNDYTAN OCH NETTOSTAMANTALET

         IF(A1.LT..00001) A1=0.
         A1=A1/20.*2.
         A2=A2/20.
         BPSJGALL=A1+A2

      RETURN
      END

      REAL FUNCTION BPSJGFAS(akl,gryta,tp,gp,th100,gh100) result(BPxSJGFAS)
!**********************************************************************
! [NDAM]L: ANDEL AV GRUNDYTAN SOM SJ[LVGALLRAS under perioden        **
! Söderberg, U. 1986. Funktioner för skogliga produktionsprognoser.
!  SLU, avd. för skogsuppskattning och skogsindelning. Rapport 14.
!
! (in) akl   = ytans total†lder
!      gryta  = grundyta (m2/ha)
!      tp     = andel tall (decimalt)
!      gp     =   "   gran (")
!      th100  = bonitet tall (dm)
!      gh100  =   "     gran (")
!  (ut) avg†ng (5 †r) andel av grundyta f”r tr„dslag i (dec.)
!       antag relative diameter = 1
!**********************************************************************
!-- formella parametrar
      REAL*4    akl,gryta,tp,gp,th100,gh100
!-- lokala deklarationer
      REAL*4    C(11),r(11),funk
      DATA C &
     /  6.09490E-1, -1.25903E+1,  3.31700E-4, -1.00600E-2,  1.73000E-4, &
        1.56000E-4, -1.30000E-2,  1.19000E-2,  1.89000E-2, -1.74000E-2, &
        2.32000E-2/

!    INVERTERAD TOTAL]LDER
      R(1)  = 1.0/(akl+10.)
!    KVADRERAD INVERTERAD TOTAL]LDER
      R(2)  = R(1)**2
!    GRUNDYTA
      R(3)  = GRYTA
!    LOGARITMERAD GRUNDYTA
      R(4)  =ALOG(R(3))
!    ST]NDORTSINDEX MED TALL SOM BONITETSVISANDE TR[DSLAG
      IF (tp .GE. 0.50) THEN
          R(5)  = 0.00
          R(6)  = TH100/10.
!    ST]NDORTSINDEX MED GRAN SOM BONITETSVISANDE TR[DSLAG
      ELSE
          R(5)  = GH100/10.
          R(6)  = 0.00
      ENDIF
!    GRANANDEL
      R(7)  = gp
!    KVADRERAD GRANANDEL
      R(8)  = R(7)**2
!    L\VANDEL
      R(9)  = 1.00-tp-gp
!    KVADRERAD L\VANDEL
      R(10) = R(9)**2
!    KONSTANT
      R(11) = 1.00

      FUNK = C( 1)*R( 1) + C( 2)*R( 2) + C( 3)*R( 3) + C( 4)*R( 4) &
           + C( 5)*R( 5) + C( 6)*R( 6) + C( 7)*R( 7) + C( 8)*R( 8) &
           + C( 9)*R( 9) + C(10)*R(10) + C(11)*R(11)

      funk = FUNK*5.
      BPxSJGFAS=max(0.0,min(1.,funk))

      RETURN
      END

      REAL*4 FUNCTION BPSJGGRY(akl,tp,gp,th100,gh100,stot) result(BPxSJGGRY)
!**********************************************************************
! [NDAM]L: GRUNDYTEGR[NS F\R SJ[LVGALLRING                          **
! Söderberg, U. 1986. Funktioner för skogliga produktionsprognoser.
!  SLU, avd. för skogsuppskattning och skogsindelning. Rapport 14.
!                                                                    **
! (in) akl   = ytans total†lder
!      tp     = andel tall (decimalt)
!      gp     =   "   gran (")
!      th100  = bonitet tall (dm)
!      gh100  =   "     gran (")
!      stot   = totalt stamantal (/ha)
! (ut) sjggry = "sj„lvgallringsgr„ns" (m2/ha)
!**********************************************************************
!-- formella parametrar
      REAL*4    akl,tp,gp,th100,gh100,stot
!-- lokala deklarationer
      REAL*4    C(11),r(11),funk
      DATA c &
     /-1.86120E+1, -7.65295E+2,  4.79800E-2,  5.58900E-2,  6.71700E-5, &
      -2.86400E-9,  7.20400E-1, -4.87900E-1,  1.06200E-1, -2.07300E-1, &
       2.52250E+0/

!    INVERTERAD TOTAL]LDER
      R(1)  = 1.0/(akl+10.0)
!    KVADRERAD INVERTERAD TOTAL]LDER
      R(2)  = R(1)**2
!    ST]NDORTSINDEX MED TALL SOM BONITETSVISANDE TR[DSLAG
      IF (tp .GE. 0.50) THEN
          R(3)  = 0.00
          R(4)  =TH100/10.
!    ST]NDORTSINDEX MED GRAN SOM BONITETSVISANDE TR[DSLAG
      ELSE
          R(3)  =GH100/10.
          R(4)  = 0.00
      ENDIF
!    STAMANTAL
      R(5)  = STOT
!    KVADRERAT STAMANTAL
      R(6)  = R(5)**2
!    GRANANDEL
      R(7)  = gp
!    KVADRERAD GRANANDEL
      R(8)  = R(7)**2
!    L\VANDEL
      R(9)  = 1.00-tp-gp
!    KVADRERAD L\VANDEL
      R(10) = R(9)**2
!    KONSTANT
      R(11) = 1.00

      FUNK = C( 1)*R( 1) + C( 2)*R( 2) + C( 3)*R( 3) + C( 4)*R( 4) &
           + C( 5)*R( 5) + C( 6)*R( 6) + C( 7)*R( 7) + C( 8)*R( 8) &
           + C( 9)*R( 9) + C(10)*R(10) + C(11)*R(11)

      BPxSJGGRY = EXP(FUNK)

      RETURN
      END

