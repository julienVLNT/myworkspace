//  First programs in C.
//  Julien VALENTIN (July, 2022)  julien.vlnt@gmail.com
//  About functions in a script.
#include <stdio.h>

/*
    Functions prototypes must be declared before the `main` function. They can
    also be declared in extern header files `*.h`
*/

// This first realizes the sum of two real numbers.
float plus(float x, float y);

// A function that assign to `x` the max of a serie `s`.
void max(float * x, float serie[], int length);

// A function that compute the extern product of two vectors
void prod(float ** mat, float *vec1, float * vec2, int length1, int length2);

/*
    MAIN
    ----
*/
int 
main()
{
    float x = 1.1;
    float y = -0.24;
    float z;
    float serie[3] = {-1.1, 2.4, 4/3};
    float * vec1;
    float * vec2;
    float * * mat;    // A matrix, the external product of vec1 and vec2

    // The first method to pass parameters to a function is by values. The function
    // can not modify its arguments, just read their values.
    z = plus(x, y);
    printf("z := x + y ; z = %f\n", z);

    // One can develop inplace functions : a function that modifies the state
    // of a parameter. Let illustrate it.
    max(&x, serie, 3);
    printf("x = %f\n", x);
    
    return 0;
}

/*
    The implementation of the functions can be placed here, or in external `.c`
    files, so called libraries.
*/

float plus(float x, float y)
{
    return x + y;
}

void max(float * x, float serie[], int length)
{   
    float maximum = -10000;    // Should be min value of float
    for(int i = 0; i < length; i++)
    {
        if(serie[i] > maximum)
        {
            maximum = serie[i];
        }
    }
    *(x) = maximum;
}