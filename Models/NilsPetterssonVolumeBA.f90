!-----------------------------------------------------------------------------------------------
!--- Here is collected Nils Pettersson functions for volume and diameter distribution functions
!-----------------------------------------------------------------------------------------------

      SUBROUTINE PetterssonYoungForestState(i,roj,SI,HDOM,N, GY,VOLYM)
!-----------------------------------------------------------------------
!  Volymsber„kning vid HDOM enligt Nils Pettersson
!-----------------------------------------------------------------------
INTEGER	::	i,roj 
REAL	::	SI,HDOM,N, GY,VOLYM
REAL	::	LNSI,LNN,LNHDOM,LNFH

      LNHDOM=LOG(HDOM)
      LNN=LOG(N)
      LNSI=LOG(SI)

if(i == 1 .and. roj == 1)then		! Cleaned pine
      IF (SI.LE.25.) volym=n/((86.5*hdom**(-1.32))+(n*0.85*hdom**(-2.00)))
      IF (SI.GT.25.) volym=n/((3422*hdom**(-3.13))+(n*0.44*hdom**(-1.77)))
      fh=EXP(-0.460+0.857*Lnhdom-0.004*Lnn)
elseif(i == 1 .and. roj == 0)then	! Uncleaned pine
      volym=n/((1873*hdom**(-2.42))+(n*0.15*hdom**(-1.42)))
      LNFH=0.167+0.705*LNHDOM-0.028*LNN
      FH=EXP(LNFH)
elseif(i == 2 .and. roj == 1)then	! Cleaned spruce
      IF (SI.LE.30) volym=n/((1.24*hdom**(0.20))+(n*1.65*hdom**(-2.25)))
      IF (SI.GT.30) volym=n/((0.37*hdom**(0.44))+(n*5.57*hdom**(-2.72)))
      fh=EXP(0.88+0.718*Lnhdom-0.113*Lnn)
elseif(i == 2 .and. roj == 0)then	! Uncleaned spruce
      VOLYM=n/((8095*hdom**(-2.8674))+(n*0.2511*hdom**(-1.6611)))
      fh=exp(-0.920+lnn*0.029+lnhdom*0.829+lnsi*0.226)
else								! Deciduous
      volym=n/((2422.*hdom**(-2.47))+(n*1.00*hdom**(-1.88)))
      fh=EXP(-0.460+0.857*Lnhdom-0.004*Lnn)
endif

fh=max(1.,fh)
GY=volym/fh

RETURN
END

      



      SUBROUTINE TALLR(SI,HDOM,N,dfh,NST,GY,VOLYM,VMEDEL,VSTAND,VSNED,VTOP)
!-----R™JD TALL --------------------------------------------------------
!  Volymsber„kning vid HDOM enligt Nils Pettersson
!-----------------------------------------------------------------------
      REAL*4	SI,HDOM,N,dfh,NST,GY,VOLYM,DG,VMEDEL,VSTAND,VSNED,VTOP,D100,		&
 				HG
      REAL*4	LNSI,LNN,LNDG,LNDGV,LNGY,LNGYT,LND800,LND400,					&
				LND100,lnhdom,LNVMEDEL,LNHG,fh,gyt,d2,dgv,d800,				&
				d400,LNVSTAND,lnVSNED,lnVTOP

      LNHDOM=LOG(HDOM)
      LNN=LOG(N)
      LNSI=LOG(SI)

      IF (SI.LE.25) volym=n/((86.5*hdom**(-1.32))+(n*0.85*hdom**(-2.00)))
      IF (SI.GT.25) volym=n/((3422*hdom**(-3.13))+(n*0.44*hdom**(-1.77)))

      nst=EXP(0.460-0.05*LnHdom+0.952*lnn)
      IF (Nst .GT. N) Nst=N

      fh=EXP(-0.460+0.857*Lnhdom-0.004*Lnn)
      fh=max(1.,fh+dfh)

      GYT=Volym/FH
      lngyt=log(gyt)
      LNGy=0.063+lngyt*0.973
      GY=EXP(LNGY)

      D2=(4*GY)/(Nst*3.1416)
      Dg=100*(D2**0.5)
      lndg=log(dg)

      lndgv=0.296+lnhdom*0.140+LNDG*0.777
      Dgv=EXP(LNDgv)

      lnd800=0.751+lnhdom*0.281+lnDG*0.502
      D800=EXP(LND800)

      lnd400=0.756+lnhdom*0.339+LNDG*0.472
      D400=EXP(LND400)

      lnd100=0.858+lnhdom*0.351+LNDG*0.459
      D100=EXP(LND100)

      LNhg=0.109+1.009*lnhdom-0.028*lnn
      HG=EXP(LNHG)
     
      LNVMEDEL=0.517+0.836*LNdg
      VMEDEL=EXP(LNVMEDEL)

      LNVSTAND=-1.346+0.707*LNhdom+0.073*lnn
      VSTAND=EXP(LNVSTAND)

      lnVSNED=-1.071+0.264*lnn
      vsned=Exp(lnvsned)
      vsned=vsned-3.

      lnVTOP=3.154-lnhdom*0.492-0.105*lnn
      VTOP=Exp(lnVTOP)
      VTOP=VTOP-3.

      RETURN
      END

      SUBROUTINE TALLP(SI,HDOM,N,dfh,NST,GY,VOLYM,VMEDEL,VSTAND,VSNED,VTOP)
!-----OR™JD, PLANTERAD TALL --------------------------------------------
!  Volymsber„kning vid HDOM enligt Nils Pettersson
!-----------------------------------------------------------------------
      REAL*4	SI,HDOM,N,dfh,NST,GY,VOLYM,DG,VMEDEL,VSTAND,VSNED,VTOP,D100,		&
				HG
      REAL*4	LNSI,LNN,LNNST,LNFH,LNDG,LNDGV,LNGY,LNGYT,LND800,LND400,			&
				LND100,lnhdom,LNVMEDEL,LNHG,fh,gyt,d2,dgv,d800,				&
				d400,LNVSTAND,lnVSNED,lnVTOP

      LNHDOM=LOG(HDOM)
      LNN=LOG(N)
      LNSI=LOG(SI)

      volym=n/((1873*hdom**(-2.42))+(n*0.15*hdom**(-1.42)))

      LNFH=0.167+0.705*LNHDOM-0.028*LNN
      FH=EXP(LNFH)
      fh=max(1.,fh+dfh)

      LNNst=-0.5256-0.216*Lnhdom+0.876*Lnn+0.579*lnsi
      NST=EXP(LNNST)
      IF( Nst. GT. N )Nst=N

      GYT=Volym/FH
      LNGYT=LOG(GYT)
      LNGY=0.1582+1.018*LNGYT-0.018*LNN
      GY=EXP(LNGY)

      D2=(4*GY)/(Nst*3.1416)
      Dg=100*(D2**0.5)
      lndg=log(dg)

      LNhg=-0.153+1.064*lnhdom-0.010*LNN
      HG=EXP(LNHG)

      LNdgv=0.292+lnhdom*0.124+LNDG*0.803
      DGV=EXP(LNDGV) 

      lnd100=1.049+lnhdom*0.242+LNDG*0.502
      D100=EXP(LND100)

      lnd400=0.922+lnhdom*0.268+LNDG*0.489
      D400=EXP(LND400)

      lnd800=0.860+lnhdom*0.301+LNDG*0.444
      D800=EXP(LND800)

      LNVMEDEL=0.3607+0.834*LNDG-0.068*LNHDOM
      VMEDEL=EXP(LNVMEDEL)

      LNVSTAND=-0.270+0.134*LNDG+0.395*LNHDOM
      VSTAND=EXP(LNVSTAND)

      LNVSNED=0.2825+0.086*LNN
      VSNED=EXP(LNVSNED)
      VSNED=VSNED-3

      LNVTOP=2.679-0.183*lnn
      VTOP=EXP(LNVTOP)
      VTOP=VTOP-3
      RETURN
      END

      SUBROUTINE GRANR(SI,HDOM,N,dfh,NST,GY,VOLYM,VMEDEL,VSTAND,VSNED,VTOP)
!-----R™JD GRAN---------------------------------------------------------
!  Volymsber„kning vid HDOM enligt Nils Pettersson
!-----------------------------------------------------------------------
      REAL*4	SI,HDOM,N,dfh,NST,GY,VOLYM,DG,VMEDEL,VSTAND,VSNED,VTOP,D100,	&
				HG
      REAL*4	LNSI,LNN,LNDG,LNDGV,LND800,LND400,LND100,lnhdom,LNVMEDEL,	&
				LNHG,fh,d2,dgv,d800,d400,LNVSTAND,lnVSNED,lnVTOP

      LNHDOM=LOG(HDOM)
      LNN=LOG(N)
      LNSI=LOG(SI)

      IF (SI.LE.30) volym=n/((1.24*hdom**(0.20))+(n*1.65*hdom**(-2.25)))
      IF (SI.GT.30) volym=n/((0.37*hdom**(0.44))+(n*5.57*hdom**(-2.72)))

      nst=EXP(0.168-0.038*LnHdom+0.99*lnn)
      IF( Nst. GT. N )Nst=N

      fh=EXP(0.88+0.718*Lnhdom-0.113*Lnn)
      fh=max(1.,fh+dfh)

      GY=Volym/FH
      D2=(4*GY)/(Nst*3.1416)
      Dg=100*(D2**0.5)
      lndg=log(dg)

      lndgv=0.194+lnhdom*0.129+LNDG*0.824
      Dgv=EXP(LNDgv)

      lnd800=0.676+lnhdom*0.352+LNDG*0.452
      D800=EXP(LND800)

      lnd400=0.591+lnhdom*0.387+LNDG*0.482
      D400=EXP(LND400)

      lnd100=0.608+lnhdom*0.441+lndg*0.463
      D100=EXP(LND100)

      LNhg=0.759+0.832*lnhdom-0.054*lnn
      HG=EXP(LNHG)

      LNVMEDEL=0.503+0.839*LNDG
      VMEDEL=EXP(LNVMEDEL)

      LNVSTAND=-2.361+0.956*lnhdom+0.122*lnn
      VSTAND=EXP(LNVSTAND)

      lnVSNED=2.397+lnhdom*0.158+LNn*0.121-lnsi*0.774
      vsned=exp(lnvsned)
      vsned=vsned-3.
     
      LNVTOP=-0.306-lnn*0.157+LNsi*0.783
      VTOP=exp(lnVTOP)
      VTOP=VTOP-3.

      RETURN
      END  

      SUBROUTINE GRANP(SI,HDOM,N,dfh,NST,GY,VOLYM,VMEDEL,VSTAND,VSNED,VTOP)
!-----OR™JD, PLANTERAD GRAN --------------------------------------------
!  Volymsber„kning vid HDOM enligt Nils Pettersson
!-----------------------------------------------------------------------
      REAL*4	SI,HDOM,N,dfh,NST,GY,VOLYM,DG,VMEDEL,VSTAND,VSNED,VTOP,D100,	&
				HG
      REAL*4	LNSI,LNN,LNDG,LNDGV,LNGY,LNGYT,LND800,LND400,				&
				LND100,lnhdom,LNVMEDEL,LNHG,fh,gyt,dgv,d800,				&
				d400,LNVSTAND,lnVSNED,lnVTOP

      LNHDOM=LOG(HDOM)
      LNN=LOG(N)
      LNSI=LOG(SI)

      VOLYM=n/((8095*hdom**(-2.8674))+(n*0.2511*hdom**(-1.6611)))

      fh=exp(-0.920+lnn*0.029+lnhdom*0.829+lnsi*0.226)
      fh=max(1.,fh+dfh)

      if (n .ge. 6000 .and. si .gt. 24)						&
     	nsT=exp(-0.579+0.180+lnn*1.050+lnhdom*0.069)
      if (n .ge. 4000 .OR. N .LT. 6000 .and. si .gt. 24)	&
     	nsT=exp(-0.579+0.180+lnn*1.050+lnhdom*0.046)
      if (n .ge. 2500 .OR. N .LT. 4000 .and. si .gt. 24)	&
     	nsT=exp(-0.579+0.180+lnn*1.050+lnhdom*0.040)
      if (n .LT. 2500 .and. si .gt. 24)						&
     	nsT=exp(-0.579+0.180+lnn*1.050)

      if (n .ge. 6000 .and. si .LE. 24)						&
     	nsT=exp(-0.579+lnn*1.050+lnhdom*0.069)
      if (n .ge. 4000 .OR. N .LT. 6000 .and. si .LE. 24)	&
     	nsT=exp(-0.579+lnn*1.050+lnhdom*0.046)
      if (n .ge. 2500 .OR. N .LT. 4000 .and. si .LE. 24)	&
     	nsT=exp(-0.579+lnn*1.050+lnhdom*0.040)
      if (n .LT. 2500 .and. si .LE. 24)						&
     	nsT=exp(-0.579+lnn*1.050)

      gyt=volym/fh
      lngyt=log(gyt)
      LNgy=0.080+lngyt*1.018-lnhdom*0.013-lnn*0.007
      GY=EXP(LNGY)

      dg=((gy*40000)/(nsT*3.14159))**0.5
      lndg=log(dg)

      LNd800=0.725+lnhdom*0.264+lndg*0.535
      D800=EXP(LNd800)

      lnd400=0.860+lnHDOM*0.205+lndG*0.572
      d400=exp(lnd400)

      lnd100=1.112+lnHDOM*0.126+lndG*0.592
      d100=exp(lnd100)

      lndgv=0.373+lnhdom*0.063+lndg*0.833
      dgv=exp(lndgv)

      lnhg=-0.417+1.0095*lnhdom-0.0558*lnn+0.2054*lnsi
      HG=EXP(LNHG)

      LNVMEDEL=0.527+0.842*LNDG
      VMEDEL=EXP(LNVMEDEL)

      LNVSTAND=2.161+0.463*LNDG-0.666*lnsi
      VSTAND=EXP(LNVSTAND)

      LNVSNED=0.902+0.128*LNn-LNsi*0.265
      VSNED=EXP(LNVSNED)
      VSNED=VSNED-3

      LNVTOP=1.202-0.190*lnn+lnsi*0.445
      VTOP=EXP(LNVTOP)
      VTOP=VTOP-3
      
      RETURN
      END

      SUBROUTINE BJK(SI,HDOM,N,dfh,NST,GY,VOLYM,VMEDEL,VSTAND,VSNED,VTOP)
!-----R™JD TALL --------------------------------------------------------
!  Volymsber„kning vid HDOM enligt Nils Pettersson
!-----------------------------------------------------------------------
      REAL*4	SI,HDOM,N,dfh,NST,GY,VOLYM,DG,VMEDEL,VSTAND,VSNED,VTOP,D100,	&
				HG
      REAL*4	LNSI,LNN,LNDG,LNDGV,LNGY,LNGYT,LND800,LND400,				&
				LND100,lnhdom,LNVMEDEL,LNHG,fh,gyt,d2,dgv,d800,			&
				d400,LNVSTAND,lnVSNED,lnVTOP

      LNHDOM=LOG(HDOM)
      LNN=LOG(N)
      LNSI=LOG(SI)

      volym=n/((2422.*hdom**(-2.47))+(n*1.00*hdom**(-1.88)))

      nst=EXP(0.460-0.05*LnHdom+0.952*lnn)
      IF (Nst .GT. N) Nst=N

      fh=EXP(-0.460+0.857*Lnhdom-0.004*Lnn)
      fh=max(1.,fh+dfh)

      GYT=Volym/FH
      lngyt=log(gyt)
      LNGy=0.063+lngyt*0.973
      GY=EXP(LNGY)

      D2=(4*GY)/(Nst*3.1416)
      Dg=100*(D2**0.5)
      lndg=log(dg)

      lndgv=0.296+lnhdom*0.140+LNDG*0.777
      Dgv=EXP(LNDgv)

      lnd800=0.751+lnhdom*0.281+lnDG*0.502
      D800=EXP(LND800)

      lnd400=0.756+lnhdom*0.339+LNDG*0.472
      D400=EXP(LND400)

      lnd100=0.858+lnhdom*0.351+LNDG*0.459
      D100=EXP(LND100)

      LNhg=0.109+1.009*lnhdom-0.028*lnn
      HG=EXP(LNHG)
     
      LNVMEDEL=0.517+0.836*LNdg
      VMEDEL=EXP(LNVMEDEL)

      LNVSTAND=-1.346+0.707*LNhdom+0.073*lnn
      VSTAND=EXP(LNVSTAND)

      lnVSNED=-1.071+0.264*lnn
      vsned=Exp(lnvsned)
      vsned=vsned-3.

      lnVTOP=3.154-lnhdom*0.492-0.105*lnn
      VTOP=Exp(lnVTOP)
      VTOP=VTOP-3.

      RETURN
      END

