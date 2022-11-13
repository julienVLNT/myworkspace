#include<stdlib.h>
#include<stdio.h>

void affichertable(int *table, int lon);
int max(int *table, int lon, int i, int j, int k);
void downheap(int *table, int lon, int i);
void heapsort(int *table, int lon);

int main(void) {
    int table[] = {3, 2, 4, 0, 9, 7, 5, 1, 6, 8};
    int lon = sizeof(table)/sizeof(int);
    printf("Liste a trier:\n");
    affichertable(table, lon);
    heapsort(table, lon);
    printf("Liste triee par heap sort:\n");
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

int max(int *table, int lon, int i, int j, int k) {
    int m = i;
    if (j<lon && table[j]>table[m]) {
        m = j;
    }
    if (k<lon && table[k]>table[m]) {
        m = k;
    }
    return m;
}

void downheap(int *table, int lon, int i) {
    while(1) {
        int j = max(table, lon, i, 2*i+1, 2*i+2);
        if (j==i) { break; }
        int var = table[i];
        table[i] = table[j];
        table[j] = var;
        i = j;
    }
}

void heapsort(int *table, int lon) {
    int i;
    for (i=(lon-2)/2; i>=0; i--) {
        downheap(table, lon, i);
    }
    for (i=0; i<lon; i++) {
        int var = table[lon-i-1];
        table[lon-i-1] = table[0];
        table[0] = var;
        downheap(table, lon-i-1, 0);
    }
}
