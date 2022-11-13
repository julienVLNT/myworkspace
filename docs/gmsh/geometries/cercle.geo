h = 0.1;

Point(1) = {0, 0, 0, h};
Point(2) = {1, 0, 0, h};
Point(3) = {-1, 0, 0, h};

Circle(1) = {2, 1, 3};
Circle(2) = {3, 1, 2};

Curve Loop(1) = {1, 2};
Plane Surface(1) = {1};
