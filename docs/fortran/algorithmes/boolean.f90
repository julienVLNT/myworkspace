!  First programs in Fortran
!  Julien VALENTIN (2022)  julien.vlnt@gmail.com
!  Basic logical manipulations in Fortran. Contrary to other languages in this
!  repo, Fortran does not accept the equivalence with integers or any other
!  built-in type.

program logic

    logical, parameter :: t = .true.

    print '("")'
    print '("Declaration")'
    print '("-----------")'
    print '("logical :: t = .true.")'
    print '("logical :: f = .false.")'

    print '("")'
    print *, "Size in memory storage", storage_size(t)

    print '("")'
    print '("Negation `.not.`")'
    print '("----------------")'
    print *, "(.not. .true.)  ?", .not. .true.
    print *, "(.not. .false.) ?", .not. .false.
    
    print '("")'
    print '("Logical Or `.or.`")'
    print '("-----------------")'
    print *, "(.true.  .or. .true.)  ?", .true.  .or. .true.
    print *, "(.true.  .or. .false.) ?", .true.  .or. .false.
    print *, "(.false. .or. .true.)  ?", .false. .or. .true.
    print *, "(.false. .or. .false.) ?", .false. .or. .false.

    print '("")'
    print '("Logical And `.and.`")'
    print '("-------------------")'
    print *, "(.true.  .and. .true.)  ?", .true.  .and. .true.
    print *, "(.true.  .and. .false.) ?", .true.  .and. .false.
    print *, "(.false. .and. .true.)  ?", .false. .and. .true.
    print *, "(.false. .and. .false.) ?", .false. .and. .false.

    print '("")'
    print '("Logical Equivalence `.eqv.`")'
    print '("---------------------------")'
    print *, "(.true.  .eqv. .true.)  ?", .true.  .eqv. .true.
    print *, "(.true.  .eqv. .false.) ?", .true.  .eqv. .false.
    print *, "(.false. .eqv. .true.)  ?", .false. .eqv. .true.
    print *, "(.false. .eqv. .false.) ?", .false. .eqv. .false.

    print '("")'
    print '("Logical Difference `.neqv.`")'
    print '("---------------------------")'
    print *, "(.true.  .neqv. .true.)  ?", .true.  .neqv. .true.
    print *, "(.true.  .neqv. .false.) ?", .true.  .neqv. .false.
    print *, "(.false. .neqv. .true.)  ?", .false. .neqv. .true.
    print *, "(.false. .neqv. .false.) ?", .false. .neqv. .false.
    
end program