#include<stdio.h>
#include<stdlib.h>

void affichertable(int *table, int lon);
void quicksort(int *table, int lon);

int main(void) {
    int table[] = {3, 2, 4, 0, 9, 7, 5, 1, 6, 8};
    int lon = sizeof(table)/sizeof(int);
    printf("Liste a trier:\n");
    affichertable(table, lon);
    quicksort(table, lon);
    printf("Liste triee par quicksort:\n");
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

void quicksort(int *table, int lon) {
    if(lon < 2) { return; }

    int piv = table[lon/2];

    int i, j;
    for(i=0, j=lon-1; ; i++, j--) {
        while (table[i] < piv) { i++; }
        while (table[j] > piv) { j--; }
        if (i >= j) { break; }
        int var = table[i];
        table[i] = table[j];
        table[j] = var;
    }

    quicksort(table, i);
    quicksort(table+i, lon-i);
}
