Real Function HarvTimeFi(Form,Tsl,NoAss, V) result(mt)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Cost of harvester in final felling and thinning from
! Tuomo Nurminen, Heikki Korpunen & Jori Uusitalo. 2006.
! Time consumption analysis of the mechanized cut-to-length harvesting system
! Silva Fennica 40(2): 335–363
! 
! Form	= 1=final felling; 2= thinning
! Tsl	= Species (1=pine, 2=spruce, 3=decidious
! NoAss	= Antal sortiment
! V		= Stem size (m3sk)
! 
! AvvTimeFi	= tim/m3sk
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
INTEGER ::	Form,Tsl,NoAss
REAL ::		V
REAL ::		t,t1,t2,t3,t4,t5,t6,t7,v1

v1=min(1.,V)

if(Form == 1)then
	t1=0.077
	t2=0.100
	t3=0.068+0.142*v1
	if(Tsl == 1)then
		t4=0.206+0.054*v1+0.308*v1*v1
	elseif(Tsl == 2)then
		t4=0.071+0.616*v1-0.180*v1*v1
	else
		t4=0.079+0.655*v1+0.174*v1*v1
	endif
	if(NoAss == 2)t4=t4+0.025
	if(NoAss == 3)t4=t4+0.038
	if(NoAss >= 4)t4=t4+0.055
	t5=0.047
	t6=0.022
	t7=0.012
elseif(Form == 2)then
	t1=0.100
	t2=0.100
	t3=0.093+0.101*v1
	t4=0.036+1.137*v1
	if(NoAss > 1)t4=t4+0.015
	t5=0.049
	t6=0.017
	t7=0.007
else
	write(*,'(a,i4)')' *** AvvCostFi: Hervesting form wrong: ', Form
	stop
endif

t=t1+t2+t3+t4+t5+t6+t7

mt=t*1.197*1.276/(60.*V)

End Function

Real Function ForwTimeFi(Form,Slog,Pwood,r) result(mt)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Cost of forwarder in final felling and thinning from
! Tuomo Nurminen, Heikki Korpunen & Jori Uusitalo. 2006.
! Time consumption analysis of the mechanized cut-to-length harvesting system
! Silva Fennica 40(2): 335–363
! 
! Form	= 1=final felling; 2= thinning
! Slog	= 1=sawlogs are forwarded; else =0
! Pwood	= 1=pulpwood is forwarded; else =0
! r		= forwarded volume (m3f/ha)
! 
! ForwTimeFi	= tim/m3f
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
INTEGER ::	Form,Slog,Pwood
REAL ::		r
REAL ::		t,t1,t2,t3,t4,t5,v,s,x1,x2,x4,z,a

if(Form ==1)then		!v=volume per load (m3)
	v=14.
	s=769.				!s=total length of strip roads (m/ha)
else
	v=11.
	s=500.
endif
x1=200.;x2=200.				! Driving empty and loaded, respectively, distance (m)
t1=(0.7123+0.0149*x1)/v
t2=(0.9347+0.0185*x2)/v
z=100.*max(1.,r)/s
a=27.					! a=driving speed while loading (m/min)
t3=100./(z*a)
if(Form == 1)then
	x4=exp(-0.447+0.300*log(z))
	t4=0.590+0.155/x4+0.060*Slog+0.825*Pwood
else
	x4=exp(-0.447+0.300*log(z)-1.281)
	t4=2.022+0.211/x4+0.755*Pwood-1.184*Slog
endif
if(Slog+Pwood == 2)then
	t5=(0.657+0.630)/2.
else
	t5=0.547*Slog+0.564*Pwood
endif

t=t1+t2+t3+t4+t5

mt=t*1.084*1.224/60.

End Function