#  First programs in Python
#  Julien VALENTIN (July, 2022)  julien.vlnt@gmail.com
#  A program that sleeps for two seconds.
import time

tic = time.time()
time.sleep(2)    # [s]
tac = time.time()

print(f"I slept for {tac-tic} s.")
