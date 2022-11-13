//  First programs in C
//  Julien VALENTIN (2022)  julien.vlnt@gmail.com
//  Compute the sum of the two first arguments and write the result in a file.
//  Usage : `computer.exe <m> <n> <ouputFile>`.
#include <stdio.h>
#include <Windows.h>

int main(int argc, char *argv[])
{
    int m
    ,   n
    ,   var;

    // If argc != 4 : exit.
    if(argc != 4)
    {
        printf("Usage : `computer.exe <m> <n> <outputfile>\n");
    }

    else
    {
        // Translate numbers into integers.
        m = atoi(argv[1]);
        n = atoi(argv[2]);
        
        // Compute the sum.
        var = m + n;

        // Try to open the file.
        FILE *f = fopen(argv[3], "w");

        if(f != NULL)    // opening in "writing" mode is a success.
        {
            // write the result
            fprintf(f, "%i", var);
            
            // close the file before the program ends.
            fclose(f);

            // wait for five seconds...
            Sleep(5000);

            // ... then delete the file.
            remove(argv[3]);
        }
        else
        {
            // Error while opening the file.
            printf("Error opening file <%s>. Exit !", argv[3]);
        }
    
    }

    return 0;
}