#include <stdio.h>

int main(){
    int inteiros_finais[10];
    
    int i, j, inteiro_atual;

    printf("Digite 10 inteiros diferentes:\n");

    for(i = 0; i < 10; i++){
        do{
            printf("Inteiro %d: ", i + 1);
            scanf("%d", &inteiro_atual);

            for(j = 0; j < i; j++){
                if(inteiro_atual == inteiros_finais[j]){
                    
                    printf("Inteiro repetido, digite outro.\n");
                    inteiro_atual = -1;
                    break;
                }
            }
        }while(inteiro_atual == -1);

        inteiros_finais[i] = inteiro_atual;
    }

    printf("Vetor final:\n");

    for(i = 0; i < 10; i++){
        printf("%d\t", inteiros_finais[i]);
    }
    return 0;
}