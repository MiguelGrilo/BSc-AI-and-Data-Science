#include <stdio.h>

void troca(int *x, int *y) {
    int swap = *x;
    *x = *y;
    *y = swap;

}

void ordena(int *v1, int *v2, int *v3) {
    if (*v1 > *v2) {
        troca(v1, v2);

    }
    if (*v2 > *v3) {
        troca(v2, v3);
    
    }
    if (*v1 > *v2) {
        troca(v1, v2);
    
    }
}

int main() {
    int v1, v2, v3;
    
    printf("Digite o primeiro número: ");
    scanf("%d", &v1);

    printf("Digite o segundo número: ");
    scanf("%d", &v2);

    printf("Digite o terceiro número: ");
    scanf("%d", &v3);

    ordena(&v1, &v2, &v3);

    printf("Valores ordenados: %d, %d, %d\n", v1, v2, v3);

    return 0;
}