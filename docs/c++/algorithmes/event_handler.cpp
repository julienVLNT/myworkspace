//  First programs in C++.
//  Julien VALENTIN (July, 2022)  julien.vlnt@gmail.com
//  Counts until the user hits a key.
#include <conio.h>
#include <iostream>
#include <Windows.h>

int main()
{
    // Initialize the counter
    int count = 0;
    
    // Counting loop
    while(true)
    {
        std::cout << count << std::endl;

        // Update
        count += 1;

        // If a key has been pressed : exit
        if(_kbhit())
        {
            std::cout << "Exiting after receiving signal : " << _getch() << std::endl;
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
