#  First programs in Julia
#  Julien VALENTIN (June 2022)  julien.vlnt@gmail.com
#  About functions in Julia

(x -> 0)                           # declaration of an anonymous function
f(t,x,y) = t*(1-sqrt(x^2+y^2))     # declaration of a function of three parameters

@doc """
Compute ``x`` squared only for floats !
"""
function f(x::Float64)::Float64
     return x^2
end

# A function with an indefinite number of arguments
function g(args...)
    for arg in args
        println(arg)
    end
end

# A function with an optional argument
function h(x, y, op=+)
    op(x, y)
end

# A function with a... ? What is that again ? A "modifier ?"
function i(x, y; op)
    if op == nothing
        x + y
    else
        op(x, y)
    end
end

println( ((t,x,y) -> t*(1-sqrt(x^2+y^2)))(1,1,-1) )    # evaluation of an anonymous function
f(1.0,1,-1)                                  # evaluation of f