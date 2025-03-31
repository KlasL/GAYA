Real Function GROTtime(ODT,A,H,L,K1) result(mt)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Brunberg, T., & Eliasson, L. (2013). 
! Underlag f�r produktionsnorm f�r grotskotare.
! Skogforsk rapport, (45-2013), 12.
! 
! ODT = Uttaget i ton/ha
! A = enkelt terr�ngtransportavst�nd, m ( < ~500)
! H = k�rhastigheten i m/min vid olika ytstruktur och lutning (standard ~50)
! L = laststorleken i m3f (standard 10)
! K1 = Konstant (standardv�rde 0.75)
! 
! AvvTimeFi	= tim/ODT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REAL ::		ODT,A,H,L,K1
REAL ::		d,m3f,tt,tk,tov
data d/0.4/ ! Densitet

m3f = ODT/d

! Terminaltid
tt = (3.79*m3f+65.)*K1/m3f

! K�rning
tk = 2.*A/(H*L)

! �vrig tid
tov = 1.5*m3f/L

mt =((tt+tk)*m3f+tov)/(60.*ODT)

End Function
