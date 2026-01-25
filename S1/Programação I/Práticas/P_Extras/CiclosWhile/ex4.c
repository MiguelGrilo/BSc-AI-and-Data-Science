#include <stdio.h>

int main(){
    float valor, soma = 0;

    while(valor != 0){
        printf("Digite o valor: ");
        scanf("%f", &valor);
        getchar();

        soma += valor;
    }

    printf("A soma é igual a: %.1f\n", soma);

    return 0;
}