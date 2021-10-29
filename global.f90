MODULE GLOBALS

implicit none

REAL(8)  :: pi_sum_squared, rel_error
REAL(8), allocatable :: pi_sum(:)
INTEGER(8) :: N

END MODULE
! These must be accessed during normal program execution and when program 
! is killed with sigint
