#include <stdio.h>
#include <stdlib.h>

int main(){
    float a;    //valor a adicionar
    
    float b;    //soma
    
    char c;     //continuar ou encerrar a ação
    
    b = 0;      //início do valor da soma

    while(1){
        printf("Digite o valor a somar:");
        scanf("%f", &a);
        getchar();

        b = b + a;

        printf("Deseja continuar?(y/n)");
        scanf("%c", &c);

        if(c == 'n'){
            break;
        }
    }

    printf("O valor total gasto é igual a: %.2f euros.\n", b);

    return 0;
}