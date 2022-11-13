!  First programs in Fortran.
!  Julien VALENTIN (2022)  julien.vlnt@gmail.com
!  A program that sleeps for two seconds.

!  TO FIX : how to capture these two seconds ? tac-tic = 0 here.
program main
    real :: tic, tac
    call time(tic)
    call sleep(2)    ! [s]
    call time(tac)
    print '("I slept for ", f6.3, " s.")', tac-tic
end program