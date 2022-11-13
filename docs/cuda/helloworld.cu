//  First programs in CUDA.
//  Julien VALENTIN (2022)  julien.vlnt@gmail.com
//  "Hello, World !" program.
//  From Visual Studio Command Prompt : `nvcc helloCUDA.cu`
#include <cuda_runtime.h>
#include <cuda.h>
#include <stdio.h>

/*
    A function that is called by the host to run on the device. Here, it does
    not do anything. The host (CPU) will call this function to run on the 
    device (GPU).
*/
__global__ void do_nothing()
{
}

int main()
{
    // The host calls the function, distribute it in 1 block (memory splitting)
    // and 1 thread (number of cores). 
    do_nothing<<<1,1>>>();

    // Finally say Hello ! LOL !
    printf("Hello, World ! I am your C.P.U !");

    return 0;
}
