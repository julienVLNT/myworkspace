#  First programs in Julia.
#  Julien VALENTIN (2022)  julien.vlnt@gmail.com
#  Compute the sum of the two first arguments and write the result in a file.
#  usage : `julia computer.jl <m> <n> <ouputFile>`

# if length(ARGS) != 3 : exit.
if length(ARGS) != 3
    println("usage : julia computer.jl <m> <n> -o <output txt file>")
else
    # Arguments are represented under the type String.
    m = parse(Int64, ARGS[1])
    n = parse(Int64, ARGS[2])
    
    # Compute the sum.
    var = m + n
    
    # Write the result in the file.
    try
        open(ARGS[3], "w") do f
            write(f, "$(var)\n")
        end
    catch
        println("Error writing in the file !")
    end

    # Sleep five seconds...
    sleep(5)

    # ... then delete the file.
    rm(ARGS[3])
end