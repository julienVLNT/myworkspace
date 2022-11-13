#include<stdio.h>
#include<stdlib.h>
#include<time.h>

void affichertable(int *table, int lon);
int * melanger(int *table, int *melangee, int lon);
int chercher(int m, int *table, int lon);

int main(void) {
    int table[10];
    int lon = sizeof(table)/sizeof(int);
    
    int *melangee = (int*)calloc(lon, sizeof(int));
    for(int i=0; i<lon; i++) {
        melangee[i] = -1;
    }

    srand(time(0));

    for(int i=0; i<lon; i++) {
        table[i] = i;
    }
    printf("Liste originale:\n");
    affichertable(table, lon);

    melanger(table, melangee, lon);

    printf("Liste melangee:\n");
    affichertable(melangee, lon);

    free(melangee);

    return 0;
}


void affichertable(int *table, int lon) {
    printf("{ ");
    for(int i=0; i<lon; i++) {
        printf("%i ", table[i]);
    }
    printf("}\n");
}

int chercher(int m, int *table, int lon) {
    int var = 0;
    for(int i=0; i<lon; i++) {
        if(m==table[i]) {
            var = 1;
            break;
        }
    }
    return var;
}

int * melanger(int *table, int *melangee, int lon) {
    int j;
    for(int i=0; i<lon; i++) {
        do {
            j = (int)rand() % lon;
            printf("Step %d...\tj = %d\n", i, j);
            affichertable(melangee, lon);
        } while(chercher(table[j], melangee, lon));
        melangee[i] = table[j];
    }
    return melangee;
}
