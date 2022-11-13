//  First try to print multiple "Hello, World !" on the terminal with the
//  PETSc library.
//  Julien VALENTIN (July, 2022)  julien.vlnt@gmail.com
//  usage : `make hello && $PETSC_DIR/lib/petsc/bin/petscmpiexec -n 8 ./hello`
#include <petscsys.h>

int main(int argc, char ** argv)
{
    PetscMPIInt rank;
    
    PetscInitialize(&argc, &argv, NULL, NULL);

    MPI_Comm_rank(PETSC_COMM_WORLD, &rank);

    PetscSynchronizedPrintf(PETSC_COMM_WORLD, "Rank %d says hello !\n", rank);
    PetscSynchronizedFlush(PETSC_COMM_WORLD, PETSC_STDOUT);

    PetscFinalize();

    return 0;
}
