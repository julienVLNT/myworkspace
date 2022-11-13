//  First programs in C++.
//  Julien VALENTIN  julien.vlnt@gmail.com
//  A program that sleeps for two seconds.
#include <iostream>
#include <Windows.h>

int main()
{
    Sleep(2000);    // [ms]
    std::cout << "I slept for two seconds !" << std::endl;

    return 0;
}
