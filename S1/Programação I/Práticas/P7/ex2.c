#include <stdio.h>
#include <stdlib.h>

int main(){
    float a;    //valor
    float b;    //soma
    float c;    //contagem
    char d;     //continuação
    float e;    //média

    do{
        printf("Digite um valor: ");
        scanf("%f", &a);
        getchar();

        printf("Deseja continuar?(s/n) ");
        scanf("%c", &d);
        getchar();

        b = b + a;

        c = c + 1;

    }while(d == 's');

    e = b/c;

    printf("A média é igual a: %.2f\n", e);

    return 0;
}