#include <stdio.h>
#include <math.h>

float montanteFinal(float principal, float taxaDeJuros, int anos, int vezesCompostos){
    float montante = principal * pow(1 + taxaDeJuros/vezesCompostos, vezesCompostos * anos);
    return montante;
}

float exibirResultado(float montanteFinal){
    printf("O montante final é igual a %.2f\n", montanteFinal);
}

int main(){
    float principal, taxaDeJuros;
    int anos, vezesCompostos;
    printf("Digite o valor principal: ");
    scanf("%f", &principal);
    getchar();

    printf("Digite a taxa de juros em decimal: ");
    scanf("%f", &taxaDeJuros);
    getchar();

    printf("Digite o número de anos: ");
    scanf("%d", &anos);
    getchar();

    printf("Digite o número de vezes que os juros são compostos por ano: ");
    scanf("%d", &vezesCompostos);
    getchar();

    float final = montanteFinal(principal, taxaDeJuros, anos, vezesCompostos);

    exibirResultado(final);

    return 0;
}