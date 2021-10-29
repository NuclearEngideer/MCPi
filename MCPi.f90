PROGRAM MCPi

USE Globals, ONLY :  N, pi_sum
USE OMP_LIB

IMPLICIT NONE

INTRINSIC SIGNAL
REAL(8), allocatable :: pi_est(:)
INTEGER(8)    :: NS, F, calc_per_thread, id, upper, lower, i
REAL(16)      :: XR,YR
INTEGER(4)    :: threads
CHARACTER(4)  :: threads_string
CHARACTER(128) :: set_thread_cmd
INTEGER(8), allocatable :: thread_div(:), success(:)

EXTERNAL SIGINT_FUNC, RESULTS, THREAD_FUNC


WRITE(*,*) 'Please enter number of points to calculate pi on'
READ(*,*) NS
WRITE(*,*) 'Enter the number of threads to calculate pi with'
READ(*,*) threads

! Write the threads integer to character for the env. variable call
write(threads_string, '(I4)') threads
set_thread_cmd = 'export OMP_NUM_THREADS='//trim(adjustl(threads_string))

allocate( pi_est( threads ) )
allocate( pi_sum( threads ) )
allocate( success( threads ) )
allocate( thread_div( threads ) )

write(*,*) set_thread_cmd 

CALL EXECUTE_COMMAND_LINE(set_thread_cmd)

pi_est=0
SUCCESS=0
PI_sum=0
! PI_sum_squared=0

WRITE(*,*) 'Calculating Pi with ', NS, ' random X,Y pairs...'
WRITE(*,*) 'With ', threads, 'threads.'

f = NS/100

! Divide the calculation across threads

calc_per_thread = NS/int(threads)
do I = 1, size(thread_div)
    thread_div(I)=calc_per_thread*(I)
enddo
if (thread_div(size(thread_div)) /= NS) thread_div(size(thread_div)) = NS

! Begin parallelization, maybe
!$OMP PARALLEL PRIVATE(id, lower, upper, N, XR, YR)

id = OMP_GET_THREAD_NUM()
if ( OMP_GET_NUM_THREADS() == 1 ) then
    lower = 1
    upper = ns
else if ( id == 0 ) then
    lower = 1
    upper = thread_div(id+1)
else
    lower = thread_div(id)+1
    upper = thread_div(id+1)
endif

do N = lower, upper
    call RANDOM_NUMBER(XR)
    call RANDOM_NUMBER(YR)
    
    if ( XR*XR+YR*YR <= 1 ) then
        success(id+1) = success(id+1) + 1
    endif
    PI_est(id+1) = 4*REAL(SUCCESS(id+1))/REAL(N)
    PI_sum(id+1)=PI_sum(id+1)+PI_est(id+1)
end do

!$OMP END PARALLEL

write(*,*) 'estimate for pi', pi_sum

write(*,*) 'alternate estimate for pi', 4*real(sum(success))/NS
! call results()

end program
    
! subroutine results()
!     use globals
!     IMPLICIT NONE
!     real(8) :: Pi
!     
!     character(*), parameter :: fmto = '(A31, I9.3, A18, F8.5, /, A20, es12.5)'
!     
!     PI=PI_sum/N  ! Calculate the estimated mean
! 
!     ! Caluclate the variance of the mean
!     rel_error = sqrt(((N/(N-1))*((Pi_sum_squared/n) - (Pi*Pi))))/Pi
! 
!     write(*, fmt=fmto) 'The value of Pi calculated from ', N, ' random points is: ', Pi, 'with relative error: ', rel_error
! end subroutine 
! 
! subroutine sigint_func()
!     use globals
!     implicit NONE 
!     external results
!     write(*,*) 'Program terminated after ', N, ' trials:' 
!     
!     call results() 
! 
!     stop
! end subroutine
! 
