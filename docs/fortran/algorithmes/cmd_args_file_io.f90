!  First programs in Fortran.
!  Julien VALENTIN (2022)  julien.vlnt@gmail.com
!  Compute the sum of the two first arguments and write the result in a file.
!  Usage : `computer.exe <m> <n> <ouputFile>`.
program computer

    ! Declare variables.
    character(len=8) :: charm, charn, filename
    integer :: m, n, var

    ! Test the number of arguments. 
    if(command_argument_count().ne.3) then
        print '("Usage : computer.exe <m> <n> <outputfile>")'  
        stop
    endif

    ! Get the arguments.
    call get_command_argument(1, charm)
    call get_command_argument(2, charn)
    call get_command_argument(3, filename)

    ! Translate charm and charn into integers.
    read(charm, *) m
    read(charn, *) n

    ! Compute the sum.
    var = m + n

    ! Write the result in the file.
    open(file=filename, unit=0)
    write(0, '(I0)') var
    
    ! Sleep for five seconds...
    call sleep(5)

    ! ... then delete the file !
    close(unit=0, status='delete')

end program