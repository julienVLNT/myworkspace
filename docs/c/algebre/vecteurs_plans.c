#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

struct Vec2 {
    double x;
    double y;
};

double dot(struct Vec2 u, struct Vec2 v) {
    double dot;
    dot = u.x*v.x + u.y*v.y;
    return dot;
}

double norm(struct Vec2 v) {
    double norm;
    norm = sqrt(pow(v.x, 2) + pow(v.y, 2));
    return norm;
}

void print(struct Vec2 u) {
    printf("Vecteur [%lf, %lf]\n", u.x, u.y);
}

struct Vec2 ones() {
    struct Vec2 u;
    u.x = 1.0;
    u.y = 1.0;
    return u;
}

struct Vec2 zeros() {
    struct Vec2 u;
    u.x = 0.0;
    u.y = 0.0;
    return u;
}

struct Vec2 randoms() {
    struct Vec2 u;
    u.x = (double)rand()/RAND_MAX;
    u.y = (double)rand()/RAND_MAX;
    return u;
}

struct Vec2 prod(double a, struct Vec2 u) {
    struct Vec2 v;
    v.x = a*u.x;
    v.y = a*u.y;
    return v;
}

struct Vec2 sum(struct Vec2 u, struct Vec2 v) {
    struct Vec2 w;
    w.x = u.x + v.y;
    w.y = u.y + v.y;
    return w;
}

int main(int argc, char *argv[]) {
    srand(time(0));
    struct Vec2 u, v, w;
    
    u = ones();
    v = randoms();
    w = zeros();
    
    printf("u :\n");
    print(u);

    printf("v :\n");
    print(v);
    printf("w :\n");
    print(w);
    printf("||u|| :\n\t%f\n", norm(u));
    printf("u dot v :\n\t%f\n", dot(u, v));
    printf("3 * u :\n");
    u = prod(3, u);
    print(u);

    return 0;
}
