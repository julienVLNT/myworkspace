mutable struct Point
    x::Vector{Float64}
end

function distance(p1::Point, p2::Point)
    sqrt(sum((p1.x - p2.x).^2))
end


mutable struct Segment
    p1::Point
    p2::Point
end

function midpoint(s::Segment)
    Point(0.5 * (s.p1.x .+ s.p2.x))
end


mutable struct Sphere
    center::Point
    other::Point
end

function radius(s::Sphere)
    distance(s.center, s.other)
end

function diameter(s::Sphere)
    2*distance(s.center, s.other)
end


mutable struct Triangle
    p1::Point
    p2::Point
    p3::Point
end
