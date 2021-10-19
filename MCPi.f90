PROGRAM MCPi

USE Globals

IMPLICIT NONE

INTRINSIC SIGNAL
REAL(8)    :: pi_est 
INTEGER(8) :: SUCCESS, NS, F
REAL(16)   :: XR,YR

EXTERNAL SIGINT_FUNC, RESULTS

WRITE(*,*) 'Please enter number of points to calculate pi on'
READ(*,*) NS

SUCCESS=0
PI_sum=0
PI_sum_squared=0

WRITE(*,*) 'Calculating Pi with ', NS, ' random X,Y pairs...'

f = NS/100

do N = 1, NS-1
    call RANDOM_NUMBER(XR) ! = RAND()
    call RANDOM_NUMBER(YR) ! = RAND()
    
    if (XR*XR+YR*YR <= 1) then
        success=success+1
    endif
    if (MOD(N,F)==0) then
         write(*,*) 'Number of points so far: ', N
    endif
    PI_est = 4*REAL(SUCCESS)/REAL(N)
    PI_sum=PI_sum+PI_est
    PI_sum_squared=Pi_sum_squared+(Pi_est*Pi_est)
    ! Sigint handling
    call signal(2, sigint_func)
end do

call results()

end program
    
subroutine results()
    use globals
    IMPLICIT NONE
    real(8) :: Pi
    
    character(*), parameter :: fmto = '(A31, I9.3, A18, F8.5, /, A20, es12.5)'
    
    PI=PI_sum/N  ! Calculate the estimated mean

    ! Caluclate the variance of the mean
    rel_error = sqrt(((N/(N-1))*((Pi_sum_squared/n) - (Pi*Pi))))/Pi

    write(*, fmt=fmto) 'The value of Pi calculated from ', N, ' random points is: ', Pi, 'with relative error: ', rel_error
end subroutine 

subroutine sigint_func()
    use globals
    implicit NONE 
    external results
    write(*,*) 'Program terminated after ', N, ' trials:' 
    
    call results() 

    stop
end subroutine
