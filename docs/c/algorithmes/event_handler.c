//  First programs in C
//  Julien VALENTIN (2022)  julien.vlnt@gmail.com
//  Count until the user hits a key.
#include <conio.h>
#include <stdbool.h>
#include <stdio.h>
#include <Windows.h>

int main()
{
    // Initialize the counter
    int count = 0;

    // Counting loop
    while(true)
    {
        printf("%i\n", count);

        // Update
        count += 1;

        // If a key has been pressed : exit
        if(_kbhit())
        {
            printf("Exiting after receiving signal : %i", _getch());
            break;
        }

        // To count slow enough...
        Sleep(1000);

        // Watchdog
        if(count > 25)
        {
            break;
        }
    }

    return 0;
}