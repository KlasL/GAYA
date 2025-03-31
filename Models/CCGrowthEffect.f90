REAL FUNCTION CCGrowthEffect(Scenario,Zone,Year,Species) result(CCGrowthEffectx)
! Funktion för relativ ökning av tillväxten
! Scenario: 0 = No growth effect; 1 = B2; 2 = A2
! Zone: geografisk zon (1= n:a Sv, 2= m:a Sv, 3= s:a Sv)
! Year from start of simulation
! Species: 1 = pine; 2 = spruce; 3 = other

INTEGER	:: Scenario,Zone,Year,Species
REAL	:: B1,CCGE100

CCGrowthEffectx = 0.0
if( Scenario == 0 )return

B1 = min( 2020. + 80., 2020. + Year )

if( scenario == 1)then
  if( Species == 2 )then
    if( Zone == 3 )then 
      CCGE100 = -0.0021208*B1*B1+8.9515*B1-9428
    elseif( Zone == 2 )then 
      CCGE100 = -0.00045426*B1*B1+2.2082*B1-2607
	else
      CCGE100 = -0.0046969*B1*B1+19.841*B1-20914
	endif
  elseif( Species == 1 )then
    if( Zone == 3 )then 
      CCGE100 = -0.0009089*B1*B1+3.9864*B1-4344.4
    elseif( Zone == 2 )then 
      CCGE100 = -0.00181821*B1*B1+7.7127*B1-8159.8
	else
      CCGE100 = -0.004091*B1*B1+17.164*B1-17978
	endif
  else
    if( Zone == 3 )then 
      CCGE100 = -0.00106065*B1*B1+4.5724*B1-4907
    elseif( Zone == 2 )then 
      CCGE100 = 0.0030304*B1*B1- 12.215*B1+12309
	else
      CCGE100 = 0.0016667*B1*B1-6.4967*B1+6322
	endif
  endif

elseif( scenario == 2)then
  if( Species == 2 )then
    if( Zone == 3 )then 
      CCGE100 = -0.0098485*B1*B1+40.747*B1-42122
    elseif( Zone == 2 )then 
      CCGE100 = -0.0093939 *B1*B1+38.946*B1-40338
	else
      CCGE100 = -0.0096970*B1*B1+40.308*B1-41853
	endif
  elseif( Species == 1 )then
    if( Zone == 3 )then 
      CCGE100 = -0.006364*B1*B1+26.501*B1-27560
    elseif( Zone == 2 )then 
      CCGE100 = -0.0054549*B1*B1+22.745*B1-23686
	else
      CCGE100 = -0.00530314*B1*B1+22.175*B1-23154
	endif
  else
    if( Zone == 3 )then 
      CCGE100 = -0.00181822*B1*B1+7.6894*B1-8113
    elseif( Zone == 2 )then 
      CCGE100 = -0.0015152*B1*B1+6.4573*B1-6861.3
	else
      CCGE100 = -0.0040913*B1*B1+17.234*B1-18118
	endif
  endif
endif

CCGrowthEffectx = max( 0., CCGE100/100. )
 
return
end




