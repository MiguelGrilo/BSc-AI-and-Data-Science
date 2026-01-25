#include <stdio.h>
#include <stdlib.h>

int main(){
    int matriz[3][3];
    
    int i, j, menor;

    printf("Digite os elementos da matriz: \n");

    for(i = 0; i < 3; i++){
        for(j = 0; j < 3; j++){
    
            printf("Para o elemento [%d][%d]: ", i + 1, j + 1);
            scanf("%d", &matriz[i][j]);
    
        }
    }

    menor = matriz[0][0];

    for(i = 0; i < 3; i++){
        for(j = 0; j < 3; j++){
   
            if(matriz[i][j] < menor){
                menor = matriz[i][j];   
            }
        }
    }

    printf("O menor número da matriz é: %d\n", menor);
    
    return 0;
    
}