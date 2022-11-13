#  First programs in Julia
#  Julien VALENTIN (2022)  julien.vlnt@gmail.com
#  Count until the user hits a key. Does not work at all ! Needs to be fixed.

# Initialize variables
global chr = ""
global var = 0

# Keystroke listener (asynchronous job)
@async while true
    global chr = read(stdin, Char)
    println("Character ", chr, " detected !")
end

# Counting loop
while true

    println(var)

    # Update
    global var += 1

    # If a key has been pressed : exit
    if chr != ""
        println("Exiting after receiving signal : ", chr)
        break
    end

    # To count slow enough...
    sleep(1)

    # Watchdog
    if var > 25
        break
    end

end