#include <stdio.h>
#include <stdlib.h>

float custosEnvio(int n, float c, float cs){
    return c + cs*(n - 1);
}

float custoEncomenda(int copias, float preço_unidade, float peso){
    float envio_primeira_copia = 3;

    float envio_proximas_copias = 0.2 * peso;

    float custo_envio_total = custosEnvio(copias, envio_primeira_copia, envio_proximas_copias);

    float custo_total = custo_envio_total + copias * preço_unidade;

    return custo_total;
}

int main(){
    int copias;
    printf("Digite o número de cópias: ");
    scanf("%d", &copias);
    getchar();

    float preço_unidade;
    printf("Digite o preço por unidade: ");
    scanf("%f", &preço_unidade);
    getchar();

    float peso;
    printf("Digite o peso do livro em Kg: ");
    scanf("%f", &peso);
    getchar();

    float encomenda = custoEncomenda(copias, preço_unidade, peso);

    printf("O custo da encomenda é igual %.2f.\n", encomenda);

    return 0;
}