     REAL*4 FUNCTION VolBrandel(typ,bgr,hoh,its,din,hin) result(VolxBrandel)
!-----------------------------------------------------------------------
!  Mindre funktioner enligt: Brandel, G. 1990. Rapport nr 26, Inst f skogs-
!  produktion.
!  F”r ek och bok enligt: Hagberg, E. och Mat‚rn, B. 1975. Volymfunktioner
!  f”r st†ende tr„d av ek och bok. Materialet och dess bearbetning.
!  Rapporter och uppsatser nr 15, Institutionen f”r skoglig matematisk
!  statistik, Skogsh”gskolan. Stockholm.
!
!  Ger volym ”ver stubbe; pb eller ub beroende p† typ.
!
!  (in)  typ   = 'pb' eller 'ub'
!        bgr   = breddgrad (o)
!        hoh   = h”jd ”ver havet (m)
!        its   = tr„dslag
!        1=TALL,2=GRAN,3=BJ\RK,4=\VR.L\V,5=BOK,6=EK (KOD 5,6 [NDR 8509)
!        d     = typ='pb' => dia pb; typ='ub' => dia ub (cm)
!        h     = h”jd (m)
!  (ut)  V1    = volym pb/ub (m3sk)
!-----------------------------------------------------------------------
      CHARACTER   typ*2
      INTEGER*4   its,ireg,its2,ityp,ibg,ihh,i
      REAL*4      bgr,hoh,din,hin,c(4,2,3,2),a(12,2,3,2)
      REAL*4      d,h

      DATA     c/							&
!             d      d+20        h     h-1.3
       1.83182,  0.07275, 2.12777, -1.09439, &   ! Tall - S:a Sv - pb
       1.94126, -0.11924, 1.80842, -0.74261, &   ! Tall - S:a Sv - ub
       2.00128, -0.47473, 2.87138, -1.61803, &   ! Gran - S:a Sv - pb
       1.97159, -0.42776, 2.84877, -1.58630, &   ! Gran -  S:a Sv - ub
       2.23818, -1.06930, 6.02015, -4.51472, &   ! Bj”rk - S:a Sv - pb
       2.17999, -0.78580, 5.08267, -3.68585, &   ! Bj”rk - S:a Sv - ub
       1.93867, -0.04966, 1.81528, -0.80910, &   ! Tall - N:a Sv - pb
       1.95783, -0.06331, 1.14967, -0.15286, &   ! Tall - N:a Sv - ub
       2.11123, -0.76342, 3.07608, -1.78237, &   ! Gran - N:a Sv - pb
       2.08706, -0.78814, 3.20560, -1.87006, &   ! Gran -  N:a Sv - ub
       2.47580, -1.40854, 5.16863, -3.77147, &   ! Bj”rk - N:a Sv - pb
       2.36594, -1.10578, 4.76151, -3.40177/   ! Bj”rk - N:a Sv - ub
      DATA     a/							&
! (hoh,gbr)=   1,1   1,2   1,3   1,4
!              2,1   2,2   2,3   2,4
!              3,1   3,2   3,3   3,4
      -1.40718, -1.41955, -1.41472, -1.41472, &  ! Tall - S:a Sv - pb
      -1.40718, -1.41955, -1.41472, -1.41472, &
      -1.40718, -1.41955, -1.41472, -1.41472, &  
      -1.23602, -1.23602, -1.23602, -1.23602, &  ! Tall - S:a Sv - ub
      -1.23602, -1.23602, -1.23602, -1.23602, &
      -1.23602, -1.23602, -1.23602, -1.23602, &  
      -1.02039, -1.02039, -1.02039, -1.02039, &  ! Gran - S:a Sv - pb
      -1.02039, -1.02039, -1.02039, -1.02039, &
      -1.02039, -1.02039, -1.02039, -1.02039, &
      -1.07676, -1.07676, -1.07676, -1.07676, &  ! Gran -  S:a Sv - ub
      -1.07676, -1.07676, -1.07676, -1.07676, &
      -1.07676, -1.07676, -1.07676, -1.07676, &
      -0.89363, -0.85480, -0.84627, -0.84627, &  ! Bj”rk - S:a Sv - pb
      -0.89363, -0.85480, -0.84627, -0.84627, &
      -0.89363, -0.85480, -0.84627, -0.84627, &
      -1.09588, -1.06101, -1.05775, -1.05775, &  ! Bj”rk - S:a Sv - ub
      -1.09588, -1.06101, -1.05775, -1.05775, &
      -1.09588, -1.06101, -1.05775, -1.05775, &
      -1.30052, -1.29068, -1.28297, -1.28213, &  ! Tall - N:a Sv - pb
      -1.30052, -1.29068, -1.28297, -1.28213, &
      -1.30052, -1.29068, -1.28297, -1.28213, &
      -1.22703, -1.22703, -1.22703, -1.22703, &  ! Tall - N:a Sv - ub
      -1.22910, -1.22910, -1.22910, -1.22910, &
      -1.23646, -1.23646, -1.23646, -1.23646, &
      -0.74910, -0.75384, -0.75549, -0.76640, &  ! Gran - N:a Sv - pb
      -0.75208, -0.75682, -0.75847, -0.76938, &
      -0.76488, -0.76962, -0.77127, -0.78218, &
      -0.74346, -0.74666, -0.74751, -0.75943, &  ! Gran -  N:a Sv - ub
      -0.74709, -0.75029, -0.75114, -0.76306, &
      -0.75667, -0.75987, -0.76072, -0.77264, &
      -0.44224, -0.44224, -0.44224, -0.44224, &  ! Bj”rk - N:a Sv - pb
      -0.44224, -0.44224, -0.44224, -0.44224, &
      -0.44224, -0.44224, -0.44224, -0.44224, &
      -0.72541, -0.72541, -0.72541, -0.72541, &  ! Bj”rk - N:a Sv - ub
      -0.72541, -0.72541, -0.72541, -0.72541, &
      -0.72541, -0.72541, -0.72541, -0.72541/

	VolxBrandel = 0.
    if(din == 0.)RETURN
      h	= max(1.4,hin) 
      if(its == 2)h=hin*0.9
      d = din 
!-- Bok (5) o ek (6) (Hagberg och Matern) ------------------------------
!-- Stamvirke, medeltal odelade stammar --------------------------------
      if(its.eq.5)then
         VolxBrandel=0.01275*d*d*h+0.12368*d*d+0.0004701*d*d*h*h+0.00622*d*h*h ! bok
      elseif(its.eq.6)then                                                    ! ek
         VolxBrandel=0.03522*d*d*h+0.08772*d*h-0.04905*d*d
!-- Tall, gran, bj”rk o ”vr. l”v (Brandel) -----------------------------
      else
         ireg=1
		if(bgr.gt.60.)ireg=2
        if(its==8)its2=1 
         its2=min(3,its)
         ityp=1
         if(typ.eq.'ub')ityp=2
   
         if(ireg.eq.1)then
            ihh=1
            if(hoh.ge.100.)ihh=2
            if(hoh.ge.300.)ihh=3
            ibg=1
            if(bgr.ge.57.)ibg=2
            if(bgr.ge.59.)ibg=3
         else
            ihh=1
            if(hoh.ge.200.)ihh=2
            if(hoh.ge.500.)ihh=3
            ibg=1
            if(bgr.ge.63.)ibg=2
            if(bgr.ge.65.)ibg=3
            if(bgr.ge.67.)ibg=4
         endif
         i=4*(ihh-1)+ibg
   
		 VolxBrandel=10.    **a(i,ityp,its2,ireg)* &
        d      **c(1,ityp,its2,ireg)* &
        (d+20.)**c(2,ityp,its2,ireg)* &
        h      **c(3,ityp,its2,ireg)* &
        (h-1.3)**c(4,ityp,its2,ireg)
     endif

      RETURN
      END

