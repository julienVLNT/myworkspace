#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void print(int dim, double vec[]){
    printf("[ ");
    for(int i=0; i<dim; i++){
        printf("%f ", vec[i]);
    }
    printf("]\n");
}

int main(int argc, char *argv[]){
    long dim = atoi(argv[1]);
    double *vec = (double*)calloc(dim, sizeof(double));
    srand(time(0));
    for(int i=0; i<dim; i++){
        vec[i] = (double)rand()/RAND_MAX;
    }
    print(dim, vec);
    free(vec);
    return 0;
}
