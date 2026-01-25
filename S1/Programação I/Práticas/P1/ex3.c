#include <stdio.h>

int main(){
    float euros;
    printf("Digite o valor em euros:");
    scanf("%f", &euros);

    float ienes;
    ienes = 134.5 * euros;

    printf("O valor %.2f euros corresponde a %.2f ienes.\n", euros, ienes);
}