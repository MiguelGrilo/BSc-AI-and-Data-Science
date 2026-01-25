#include <stdio.h>
#include <stdlib.h>

int main(){
    float a;        //saldo inicial/atual
        a = 1000;
    
    int b;          //opção
        b = -1;

    while(b != 0){
        printf("Escolha uma opção: \n1. Consultar saldo\n2. Fazer levantamento\n3. Fazer depósito\n0. Para sair\n");
        scanf("%d", &b);
        getchar();

        switch (b){
            case 1 :
                printf("O saldo atual é: %.2f\n", a);
                break;

            case 2 :
                float c;  //valor do levantamento
                printf("Valor a ser levantado: ");
                scanf("%f", &c);
                getchar();

                if(c <= a){
                    printf("Levantamento de %.2f euros realizado com sucesso.\n", c);
                    a = a - c;

                }else{
                    printf("Saldo insuficiente.\n");
                }
                break;

            case 3 :
                float d;  //valor a ser depositado
                printf("Valor a ser depositado: ");
                scanf("%f", &d);
                getchar();

                a = a + d;

                printf("Depósito de %.2f euros realizado com sucesso, seu saldo atual é de %.2f euros.\n", d, a);
                break;
            
            default:
                printf("Opção inválida, tente novamente.\n");
                break;
        }
    }
    return 0;
}