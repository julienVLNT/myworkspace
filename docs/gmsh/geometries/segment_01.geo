// Parametres numeriques
a = -1;
b =  1;

// Description geometrique des extremites du segment et labels
Point(1) = {a, 0, 0, h};
Physical Point("left") = {1};

Point(2) = {b, 0, 0, h};
Physical Point("right") = {2};

// Description geometrique du segment, et label
Line(1) = {1, 2};
Physical Line("Omega") = {1};
