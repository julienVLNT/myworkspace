#  First programs in Python
#  Julien VALENTIN (June 2022)  julien.vlnt@gmail.com
#  About functions and lambda expressions

# Lambda expressions are pretty useful when using very small expressions.
a, b, c = -1, 1, 1.    # three coefficients
chi = lambda x : a*x**2 + b*x + c
print(type(chi))
print("chi(-1) =", chi(-1))
print("----------")

# `def` keyword to define a function
def chi(x):
    return a*x**2 + b*x + c
print(type(chi))
print("chi(1) =", chi(1))
print("----------")

# Generally, a function comes with a documentation.
def chi(x: float) -> float:
    """Function that computes a*x**2 + b*x + c
    params : x (float), the variable of the function
    --------
    
    returns : chi(x) (float)
    ---------
    
    example
    -------
    > chi(-1)
    -1
    """
    return a*x**2 + b*x + c

print(type(chi))
print("chi(1j) = ", chi(1j))

# One may introduce default values for some parameters, these arguments become 
# facultative.
def chi(x : float = 1):
    return a*x**2 + b*x + c

print("chi() = ", chi())
