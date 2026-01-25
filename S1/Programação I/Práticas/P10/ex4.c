#include <stdio.h>
#include <stdlib.h>

int main(){
    int x[10];
    
    int y[10];

    int i;

    for(i = 0; i < 10; i++){
        printf("Para i = %d: \n", i);
        printf("Digite um inteiro para x: ");
        scanf("%d", &x[i]);
        getchar();

        printf("Digite um inteiro para y: ");
        scanf("%d", &y[i]);
        getchar();
    }

    int z[10];

    printf("Para z = x - y:\n");

    for(i = 0; i < 10; i++){
        z[i] = x[i] - y[i];
        printf("\tNa posição %d, z = %d\n", i + 1, z[i]);
    }

    return 0;
}