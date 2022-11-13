#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define PI 3.14159

struct Mat2 {
    double l0[2];
    double l1[2];
};

struct Vec2 {
    double x;
    double y;
};

void printMat(struct Mat2 P) {
    printf("[ %f %f ]\n", P.l0[0], P.l0[1]);
    printf("[ %f %f ]\n", P.l1[0], P.l1[1]);
    printf("\n");
}

void printVec(struct Vec2 u) {
    printf("[ %f %f ].T\n", u.x, u.y);
}

struct Mat2 zeros() {
    struct Mat2 P;
    P.l0[0] = 0.0;
    P.l0[1] = 0.0;
    P.l1[0] = 0.0;
    P.l1[1] = 0.0;
    return P;
}

struct Mat2 ones() {
    struct Mat2 P;
    P.l0[0] = 1.0;
    P.l0[1] = 1.0;
    P.l1[0] = 1.0;
    P.l1[1] = 1.0;
    return P;
}

struct Mat2 identity() {
    struct Mat2 P;
    P.l0[0] = 1.0;
    P.l0[1] = 0.0;
    P.l1[0] = 0.0;
    P.l1[1] = 1.0;
    return P;
}

struct Mat2 randoms() {
    struct Mat2 P;
    P.l0[0] = (double)rand()/RAND_MAX;
    P.l0[1] = (double)rand()/RAND_MAX;
    P.l1[0] = (double)rand()/RAND_MAX;
    P.l1[1] = (double)rand()/RAND_MAX;
    return P;
}

struct Mat2 rotation(double theta) {
    struct Mat2 P;
    P.l0[0] =  cos(theta);
    P.l0[1] =  sin(theta);
    P.l1[0] = -sin(theta);
    P.l1[1] =  cos(theta);
    return P;
}

struct Mat2 transpose(struct Mat2 P) {
    struct Mat2 Q;
    Q.l0[0] = P.l0[0];
    Q.l0[1] = P.l1[0];
    Q.l1[0] = P.l0[1];
    Q.l1[1] = P.l1[1];
    return Q;
}

struct Mat2 sum(struct Mat2 P, struct Mat2 Q) {
    struct Mat2 R;
    R.l0[0] = P.l0[0] + Q.l0[0];
    R.l0[1] = P.l0[1] + Q.l0[1];
    R.l1[0] = P.l1[0] + Q.l1[0];
    R.l1[1] = P.l1[1] + Q.l1[1];
    return R;
}

struct Mat2 prod(struct Mat2 P, struct Mat2 Q) {
    struct Mat2 R;
    R.l0[0] = P.l0[0]*Q.l0[0] + P.l0[1]*Q.l1[0];
    R.l0[1] = P.l0[0]*Q.l0[1] + P.l0[1]*Q.l1[1];
    R.l1[0] = P.l1[0]*Q.l0[0] + P.l1[1]*Q.l1[0];
    R.l1[1] = P.l1[0]*Q.l1[0] + P.l1[1]*Q.l1[1];
    return R; 
}

struct Mat2 dot(struct Mat2 P, double a) {
    struct Mat2 Q;
    Q.l0[0] = a*P.l0[0];
    Q.l0[1] = a*P.l0[1];
    Q.l1[0] = a*P.l1[0];
    Q.l1[1] = a*P.l1[1];
    return Q;
}

struct Vec2 apply(struct Vec2 u, struct Mat2 P) {
    struct Vec2 v;
    v.x = P.l0[0]*u.x + P.l0[1]*u.y;
    v.y = P.l1[0]*u.x + P.l1[1]*u.y;
    return v;
}

int main(int argc, char **argv) {

    struct Mat2 P, Q, R, S, T;
    struct Vec2 u, v;
    double theta;

    theta = PI/4;

    u.x = 1.0/sqrt(2.0);
    u.y = 1.0/sqrt(2.0);
    
    srand(time(0));

    P = zeros();
    printf("Matrice nulle:\n");
    printMat(P);

    Q = ones();
    printf("Matrice 1:\n");
    printMat(Q);

    R = identity();
    printf("Matrice Id:\n");
    printMat(R);

    S = randoms();
    printf("Matrice aleatoire:\n");
    printMat(S);

    T = rotation(theta);
    printf("Matrice rotation:\n");
    printMat(T);

    T = transpose(T);
    printf("T.transpose:\n");
    printMat(T);

    printf("Vecteur u:\n");
    printVec(u);

    v = apply(u, T);
    printf("Vecteur T*[1,1]\n");
    printVec(v);

    return 0;
}
