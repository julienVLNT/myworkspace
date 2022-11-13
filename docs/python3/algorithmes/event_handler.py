#  First programs in Python
#  Julien VALENTIN (July, 2022)  julien.vlnt@gmail.com
#  Count until the user hits a key.
from pynput import keyboard
from time import sleep

# Boolean `kill` initialized to false.
kill = False


# Function called when a key is pressed.
def on_press(key):
    "Turns a boolean variable to True when a key is pressed."
    
    # Change the scope
    global kill

    print("A key has been pressed.")
    kill = True

    return False


# Instance of the listener
listener = keyboard.Listener(on_press=on_press)

# Start the listener
listener.start()

# Declare the counter
count = 0

# Start to count...
while(not kill):

    print(count)

    # Update
    count += 1
    
    # To count slow enough...
    sleep(1)

    # Watchdog
    if(count > 100):
        break

# What does it mean ?
listener.join()
