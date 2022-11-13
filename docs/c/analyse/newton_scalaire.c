#include<math.h>
#include<stdio.h>
#include<stdlib.h>

double f(double x) {
    return pow(x, 3)-x;
}

double fprime(double x) {
    return 3*pow(x, 2)-1;
}

int main(int argc, char **argv) {
    int iter;
    double err, tol, x, xp;

    iter = 100;
    tol  = 1e-9;
    xp   = 5.0;

    for(int i=0; i<iter; i++) {
        x = xp - f(xp)/fprime(xp);
        err = fabs((x-xp));
        printf("Iteration %d : err = %.6f\n", i, err);
        if(err < tol) { break; }
        xp = x;
    }

    printf("x = %.9f => x^3 - x = 0\n", x);

    return 0;
}
