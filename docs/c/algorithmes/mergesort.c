#include<stdio.h>
#include<stdlib.h>

void affichertable(int *table, int lon);
void fusionner(int *table, int m, int n);
void mergesort(int *table, int lon);


int main(void) {
    int table[] = {3, 2, 4, 0, 9, 7, 5, 1, 6, 8};
    int lon = sizeof(table)/sizeof(int);
    printf("Liste a trier:\n");
    affichertable(table, lon);
    mergesort(table, lon);
    printf("Liste triee par merge sort:\n");
    affichertable(table, lon);
    return 0;
}


void affichertable(int *table, int lon) {
    printf("{ ");
    for(int i=0; i<lon; i++) {
        printf("%i ", table[i]);
    }
    printf("}\n");
}

void fusionner(int *table, int lon, int m) {
    int i, j, k;
    int *tmp = (int*)calloc(lon, sizeof(int));
    for(i=0, j=m, k=0; k<lon; k++) {
        tmp[k] = j==lon ? table[i++]
               : i==m   ? table[j++]
               : table[j] < table[i] ? table[j++]
               :                       table[i++];
    }
    for(i=0; i<lon; i++) {
        table[i] = tmp[i];
    }
    free(tmp);
}

void mergesort(int *table, int lon) {
    if(lon<2) { return; }
    int m = lon/2;
    mergesort(table, m);
    mergesort(table+m, lon-m);
    fusionner(table, lon, m);
}
