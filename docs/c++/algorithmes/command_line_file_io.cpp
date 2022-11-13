//  First programs in C++.
//  Julien VALENTIN (July 2022)  julien.vlnt@gmail.com
//  Compute the sum of the two first arguments and write the result in a file.
//  Usage : `computer.exe <m> <n> <ouputFile>`.
#include <fstream>
#include <iostream>
#include <string.h>
#include <Windows.h>

int main(int argc, char *argv[])
{
    // If argc is not equal to 4, exit.
    if(argc != 4)
    {
        std::cout << "Usage : computer.exe <m> <n> <outputFile>" << std::endl;
    }
    
    else
    {
        // Translate strings to integers.
        int m = strtol(argv[1], nullptr, 0);
        int n = strtol(argv[2], nullptr, 0);

        // Compute the sum.
        int var = m + n;

        // Write the result in a file.
        std::ofstream f;

        f.open(argv[3]);
        f << var << std::endl;
        f.close();        
    }

    // Sleep for five seconds...
    Sleep(10000);

    // ... then delete the file.
    std::remove(argv[3]);

    return 0;
}
