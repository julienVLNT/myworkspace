#include "include/finclude/petsc.h"

program hello

    integer ierr, rank    ! useful integers

    ! Start 
    call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
    
    ! Read from `com_world` the rank of the current job
    call MPI_Comm_rank(PETSC_COMM_WORLD, rank, ierr)
    
    ! Print hello, one per job
    call PetscSynchronizedPrintf(PETSC_COMM_WORLD, "Rank %d says hello !\n", rank);
    
    ! Synchronization of printing
    call PetscSynchronizedFlush(PETSC_COMM_WORLD, PETSC_STDOUT);
    
    ! End of the program
    call PetscFinalize(ierr)
end program
