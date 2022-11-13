//  First programs in C
//  Julien VALENTIN (June 2022)  julien.vlnt@gmail.com
//  About pointers in C.
#include <stdio.h>

int main()
{

    int * ptr = NULL;    // initialization
    int i = -1;
    ptr = &i;    // `&` referes to the address of i
    int j = *(ptr);    // retrieves the value at the address `ptr`
    printf("The value %d is located at %p.\n", j, ptr);

    return 0;
}