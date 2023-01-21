include("./geo.jl")

p1 = Point([0., 0., 0.])
p2 = Point([1., 0., 0.])
dist = distance(p1,p2)

s = Segment(p1, p2)
smid = midpoint(s)

s = Sphere(p1, p2)
srad  = radius(s)
sdiam = diameter(s)

p3 = Point([1., 1., 0.])
t  = Triangle(p1, p2, p3)

println("Normal end.")
