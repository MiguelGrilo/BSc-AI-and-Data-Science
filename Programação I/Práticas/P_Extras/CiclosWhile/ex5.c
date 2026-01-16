#include <stdio.h>

int main(){
    float valor, contagem = 0, soma, media;

    while(valor != 0){
        printf("Digite o valor: ");
        scanf("%f", &valor);
        getchar();
        
        if(valor != 0){
        contagem++;
        soma += valor;
        }
    }

    media = soma / contagem;

    printf("Valores introduzidos: %.0f\n", contagem);
    printf("Média: %.1f\n", media);

    return 0;
}