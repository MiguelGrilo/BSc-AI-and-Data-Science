#include <stdio.h>
#include <stdlib.h>

int main(){
    int vetor[10];

    int n[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    int i, j;

    for(i = 0; i <= 9; i++){
        printf("Digite um inteiro para a posição %d: ", i + 1);
        scanf("%d", &vetor[i]);
    }

    printf("Valores iguais:");
    for(i = 0; i <= 9; i++){
        for(j = i + 1; j <= 10; j++){
            if(vetor[i] == vetor[j]){
                printf("%d\n", vetor[i]);
                break;
            }
        }
    }

    return 0;
}