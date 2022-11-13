h = 0.1;

Point(1) = {0, 0, 0, h};
Point(2) = {1, 0, 0, h};
Point(3) = {-0.5, 0, 0, h};
Point(4) = {0.5, 0, 0, h};
Point(5) = {0, 0.5, 0, h};
Point(6) = {0, 0.5, 0, h};

Ellipsis(8) = {5, 1, 2, 3};
Ellipsis(9) = {4, 1, 2, 5};
Ellipsis(10) = {6, 1, 2, 3};
Ellipsis(11) = {4, 1, 2, 6};

Line Loop(12) = {8, -10, -11, 9};
Plane Surface(13) = {12};
