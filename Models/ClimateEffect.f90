REAL FUNCTION ClimateEffect(Growth, Years)
!***********************************************************************
! Adjusts the growth based on a yearly climate effect.
! Decreases the yearly growth by 8%.
!
! PARAMETERS:
!   Growth = (in) Initial growth value (e.g., basal area or volume increment).
!   Years  = (in) Number of years over which the growth is adjusted.
!   ClimateEffect = (out) Adjusted growth after applying the climate effect.
!***********************************************************************

    IMPLICIT NONE
    REAL, INTENT(IN) :: Growth   ! Initial growth value
    INTEGER, INTENT(IN) :: Years ! Number of years for adjustment
    REAL :: YearlyReduction      ! Yearly reduction factor

    ! Define the yearly reduction factor (8% decrease per year)
    YearlyReduction = 0.92  ! 1 - 0.08 (8% decrease)

    ! Apply the climate effect over the specified number of years
    ClimateEffect = Growth * (YearlyReduction ** Years)

    RETURN
END FUNCTION ClimateEffect
