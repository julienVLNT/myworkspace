#include<stdio.h>

void affichertable(int *table, int lon);
void bubblesort(int *table, int lon);

int main(void) {
    int table[] = {3, 2, 4, 0, 9, 7, 5, 1, 6, 8};
    int lon = sizeof(table)/sizeof(int);
    printf("Liste a trier:\n");
    affichertable(table, lon);
    bubblesort(table, lon);
    printf("Liste triee par bubble sort:\n");
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

void bubblesort(int *table, int lon) {
    int j = lon, tmp, s = 1;
    while(s) {
        s = 0;
        for(int i=1; i<j; i++) {
            if(table[i-1] > table[i]) {
                tmp = table[i];
                table[i] = table[i-1];
                table[i-1] = tmp;
                s = 1;
            }
        }
        j--;
    }
}
