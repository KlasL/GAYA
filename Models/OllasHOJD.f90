      FUNCTION G3OllasHOJD(ITR,H1,D1,D2)
!***********************************************************************
!        BERéKNING AV HôJD VID OLIKA DIAMETRAR MED OLLAS METOD. I GAYA
!        ANVéNDS GRUNDYTEMEDELSTAMMENS DIAMETER PB I STéLLET FôR DGV UB.
!
!        IN: ITR = TRéDSLAG (2 => GRAN; ANNARS GèR DET SOM TALL)
!            H1  = HôJD (M)
!            D1  = DIAMETER SVARANDE MOT H1 (CM)
!            D2  = DIAMETER FôR VILKEN HôJD SKALL BERéKNAS (CM)
!
!        UT: G2HOJD = HôJD SVARANDE MOT D2 (M)
!
!***********************************************************************
	INTEGER	:: ITR
    REAL	:: H1,D1,D2
    
      REAL K(2,4)
      DATA K/1.518,9.022,-1.086,-6.454,-.518,-1.256,1.086,1.613/
      I=1
      IF(ITR.EQ.2)I=2
      HK=(H1-K(I,1)-K(I,2)*LOG10(D1))/(K(I,3)+K(I,4)*LOG10(D1))
      G3OllasHOJD=K(I,1)+K(I,2)*LOG10(D2)+K(I,3)*HK+K(I,4)*HK*LOG10(D2)
      RETURN
      END

      FUNCTION G3HOJDx(ITR,H1,D1,D2)
!***********************************************************************
!        BERéKNING AV HôJD VID OLIKA DIAMETRAR MED OLLAS METOD. I GAYA
!        ANVéNDS GRUNDYTEMEDELSTAMMENS DIAMETER PB I STéLLET FôR DGV UB.
!
!        IN: ITR = TRéDSLAG (2 => GRAN; ANNARS GèR DET SOM TALL)
!            H1  = HôJD (M)
!            D1  = DIAMETER SVARANDE MOT H1 (CM)
!            D2  = DIAMETER FôR VILKEN HôJD SKALL BERéKNAS (CM)
!
!        UT: G2HOJD = HôJD SVARANDE MOT D2 (M)
!
!***********************************************************************
	INTEGER	:: ITR
    REAL	:: H1,D1,D2
    
      REAL K(2,4)
      DATA K/1.518,9.022,-1.086,-6.454,-.518,-1.256,1.086,1.613/
      I=1
      IF(ITR.EQ.2)I=2
      HK=(H1-K(I,1)-K(I,2)*LOG10(D1))/(K(I,3)+K(I,4)*LOG10(D1))
      G3HOJD=K(I,1)+K(I,2)*LOG10(D2)+K(I,3)*HK+K(I,4)*HK*LOG10(D2)
      RETURN
      END

