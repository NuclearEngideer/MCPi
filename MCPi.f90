PROGRAM MCPi

USE Globals, ONLY :  N, pi_sum
USE OMP_LIB

IMPLICIT NONE

INTRINSIC SIGNAL
REAL(8), allocatable :: pi_est(:)
INTEGER(8)    :: NS, F, calc_per_thread, id, upper, lower, i
REAL(16)      :: XR,YR
INTEGER(4)    :: threads
INTEGER(8), allocatable :: thread_div(:), success(:)
REAL :: start, finish

EXTERNAL SIGINT_FUNC, RESULTS, THREAD_FUNC


WRITE(*,*) 'Please enter number of points to calculate pi on'
READ(*,*) NS
WRITE(*,*) 'Enter the number of threads to calculate pi with'
READ(*,*) threads

!$ CALL OMP_SET_NUM_THREADS( threads )

allocate( pi_est( threads ) )
allocate( pi_sum( threads ) )
allocate( success( threads ) )
allocate( thread_div( threads ) )

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

call cpu_time(start)

! Begin parallel loop
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
end do

!$OMP END PARALLEL

call cpu_time(finish)

write(*,*) 'estimate for pi', 4*real(sum(success))/NS

write(*,*) 'calculation took', (finish-start)/threads, 'seconds'

end program
