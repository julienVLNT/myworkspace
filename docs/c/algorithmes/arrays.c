//  First programs in C
//  Julien VALENTIN (June 2022)  julien.vlnt@gmail.com
//  Basic and dynamic arrays in C.
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Function that checks if a character is in the word.
int in(char c, int wordLength, char word[])
{
    // c in word ? 1 : 0
    for(int i = 0; i < wordLength; i++)
    {
        if(word[i] == c)
        {
            return 1;
        }
    }
    return 0;
}

// Function that adds 2 to each components of an array of integers, inplace.
void add2(int vecLength, int * vec)
{
    for(int i = 0; i < vecLength; i++)
    {
        vec[i] += 2;
    }
}

int main()
{
    // Seed the pseudo-random generator
    srand(time(NULL));

    /*
        Arrays represent homogeneous collections of a certain type or struct.
        Let see the basics of arrays.
    */
    
    // The syntax to allocate an array is `data_type vec_name[length];` 
    int iVec[3];
    float fVec[3];
    // It is possible to assign values at declaration.
    char cVec[14] = {'H', 'e', 'l', 'l', 'o', ',', ' ', 'W', 'o', 'r', 'l', 'd', ' ', '!'};

    // One can run through an array with a `for` loop
    int iVecLength = sizeof(iVec)/sizeof(iVec[0]);
    int i = 0;
    for(i=0; i < iVecLength; i++)
    {
        // Assign data to the i-th component
        iVec[i] = i;
        // Access to the i-th component
        printf("%i\n", iVec[i]);
    }

    // Check if "o" is in "Hello, World !"
    int is = in('o', sizeof(cVec)/sizeof(char), cVec);
    if(is)
    {
        printf("The letter 'o' is in \"Hello, World !\"");
    }
    else
    {
        printf("The function isn't correct. 'o' has not been found !");
    }

    /*
        Another kind of arrays is the Variable Length Array data structure.
        This time, we use `malloc` and `free` instructions to make array
        manipulations more flexible. It becomes possible to think to adding
        or removing components.
    */
    int n = 3;
    int* ptrVec = (int*) malloc(n*sizeof(int));
    if(ptrVec == NULL)
    {
        printf("Unable to allocate memory ! Exiting.\n");
        return -1;
    }
    else
    {
        printf("%d bytes allocated for the array.\n", n);
    }
    // Go through its values : `malloc` initializes with 0s
    for(int i=0; i < n; i++)
    {
        printf("%d, ", ptrVec[i]);
    }
    printf("\n");
    // Never forget to free what has been allocated !
    free(ptrVec);

    /*
        A third way to allocate memory for arrays is the use of `calloc`, c for
        contiguous. This instruction does not allocate a whole block at once, 
        like `malloc`, but several blocks of the size of the target data type.
    */
    int* ptrVec2 = (int*) calloc(n, sizeof(int));
    if(ptrVec2 == NULL)
    {
        printf("Unable to allocate memory ! Exit.");
        return -1;
    }
    else
    {   // Modify, assign... Do anything with the vector.
        add2(n, ptrVec2);

        // Eventually print its components
        printf("[ ");
        for(int i=0; i < n-1; i++)
        {
            printf("%d, ", *(ptrVec2 + i));
        }
        printf("%d ]", ptrVec2[n-1]);
    }
    // Always end freeing the memory !
    free(ptrVec2);

    /*
        A matrix can ben represented by an array of arrays. Let see an example
        of m x n matrix.  
    */
    int mat[3][3]
    = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };
    printf("\n\n");
    for(int i=0; i < 3; i++)
    {   
        printf("[");
        for(int j=0; j < 3; j++)
        {
            printf(" %d ", mat[i][j]);
        }
        printf("]\n");
    }
    printf("\n");

    /*
        `calloc` may also be used to declare such data structures. Just like
        before, it allows to choose the size after the compilation.
    */
    // Each row is allocated
    int** ptrMat = (int**) calloc(3, sizeof(int*));
    // Allocate each column, m times, m being the number of rows.
    for(int i=0; i < 3; i++)
    {
        ptrMat[i] = (int*) calloc(5, sizeof(int));
    }
    // Go through the components of the matrix.
    for(int i=0; i < 3; i++)
    {
        printf("[ ");
        for(int j=0; j < 5; j++)
        {
            printf("%d ", ptrMat[i][j]);
        }
        printf("]\n");
    }
    // Always free what is allocated !
    free(ptrMat);

    return 0;
}
