!  First programs in Fortran
!  Julien VALENTIN (2022)  julien.vlnt@gmail.com
!  Count until the user hits a key.
program Count
    
    use ifcore

    ! Initialize the counter
    integer :: var = 0

    ! Counting loop
    do while(.true.)
        
        print '(I0)', var

        ! Update
        var = var + 1

        ! If a key has been pressed : exit
        if(peekcharqq()) then
            print *, "Terminate after receiving signal : ", getcharqq()
            call abort()
        end if

        ! To count slow enough...
        call sleep(1)

        ! Watchdog
        if(var > 25) then
            exit
        end if

    end do

end program