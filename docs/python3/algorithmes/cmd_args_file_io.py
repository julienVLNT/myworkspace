#  First programs in Python
#  Julien VALENTIN (2022)  julien.vlnt@gmail.com
#  Compute the sum of the two first arguments and write the result in the file,
#  its name being the third argument.
#  Run in a terminal : `python computer.py <m> <n> <ouputFile>`
import os
import sys
import time

# If the number is not equal to three : exit.
if( len(sys.argv) != 4 ):
    print("Usage : `python computer.py <m> <n> <outputFile>`")

else:
    # Get the two values for the sum.
    m = int(sys.argv[1])
    n = int(sys.argv[2])

    # Compute the sum.
    var = m + n

    # Write it in the specified file.
    try:
        with open(sys.argv[3], "w") as f:
            f.write(f"{var}")
    except:
        print("Error writing in the file !")

    # Wait for five seconds...
    time.sleep(5)

    # ... then delete the file.
    os.remove(sys.argv[3])
