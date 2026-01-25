#include <stdio.h>

struct Artigo{
    char tipo_de_artigo[100];
    char marca[100];
    char modelo[100];
    float preco;
    int quantidade;
};

int main(){
    struct Artigo produto1;

    printf("Tipo de artigo: ");
    fgets(produto1.tipo_de_artigo, sizeof(produto1.tipo_de_artigo), stdin);

    printf("Marca: ");
    fgets(produto1.marca, sizeof(produto1.marca), stdin);

    printf("Modelo: ");
    fgets(produto1.modelo, sizeof(produto1.modelo), stdin);

    printf("Preço: ");
    scanf("%f", &produto1.preco);

    printf("Quantidade disponível no armazém: ");
    scanf("%d", &produto1.quantidade);


    printf("\nDados do Artigo\n");
    printf("Tipo: %s", produto1.tipo_de_artigo);
    printf("Marca: %s", produto1.marca);
    printf("Modelo: %s", produto1.modelo);
    printf("Preço: %.2f\n", produto1.preco);
    printf("Quantidade disponível: %d\n", produto1.quantidade);

    return 0;
}