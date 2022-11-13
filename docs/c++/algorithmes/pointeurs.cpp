//  First programs in C++
//  Julien VALENTIN (June 2022)  julien.vlnt@gmail.com
//  About pointers in C++.
#include <iostream>

int main()
{
    /*
        Let introduce raw pointers. Here, we initialize a pointer, with the
        value NULL (else, it would receive a random value). Then, we assign to
        this pointer the address of an int variable ; one can retrieve the 
        value that is at this address.
    */
    int * ptr = nullptr;
    int i = 5;
    ptr = &i;
    int j = *ptr;
    std::cout << "The value of i is " << i << " and its address is " << ptr << " ; the value stored at the address p is " << j << std::endl;
    
    // A pointer can be incremented or decremented.
    ptr = nullptr;
    int intList[5] {1, 2, 3, 4, 5};
    ptr = &intList[0];

    std::cout << std::endl;
    for(int j=0; j < sizeof(intList)/sizeof(int); j++)
    {
        std::cout << "intList[" << j << "] = " << *(ptr++) << std::endl;
    }

    /*
        `const` and `volatile` values, pointers, or both. The keyword `const`
        specifies that the object can not be modified after initialization.
        `volatile` means that the variable can be modified even by other 
        applications.
    */
    const char cch = 'A';
    char ch = 'B';

    const char *       pch1 = &cch;
    const char * const pch2 = &cch;
    const char *       pch3 = &ch;
          char *       pch4 = &ch;
          char * const pch7 = &ch;
    const char * const pch8 = &ch;      
    // `char * pch2 = &cch;` is not allowed
    // `char *const pch3 = &cch;` raises an error too

    return 0;

}
