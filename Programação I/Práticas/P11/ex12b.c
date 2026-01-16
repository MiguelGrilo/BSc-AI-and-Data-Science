#include <stdio.h>
#include <string.h>

struct Artigo{
    char tipo_de_artigo[100];
    char marca[100];
    char modelo[100];
    float preco;
    int quantidade;
};

struct Artigo le_artigo(){
    struct Artigo novoArtigo;

    printf("Tipo de artigo: ");
    fgets(novoArtigo.tipo_de_artigo, sizeof(novoArtigo.tipo_de_artigo), stdin);
    novoArtigo.tipo_de_artigo[strcspn(novoArtigo.tipo_de_artigo, "\n")] = '\0';

    printf("Marca: ");
    fgets(novoArtigo.marca, sizeof(novoArtigo.marca), stdin);
    novoArtigo.marca[strcspn(novoArtigo.marca, "\n")] = '\0';

    printf("Modelo: ");
    fgets(novoArtigo.modelo, sizeof(novoArtigo.modelo), stdin);
    novoArtigo.modelo[strcspn(novoArtigo.modelo, "\n")] = '\0';

    printf("Preço: ");
    scanf("%f", &novoArtigo.preco);

    printf("Quantidade disponível no armazém: ");
    scanf("%d", &novoArtigo.quantidade);

    while(getchar() != '\n');

    return novoArtigo;
}

int main(){
    struct Artigo produto1 = le_artigo();;

    printf("\nDados do Artigo:\n");
    printf("\tTipo: %s\n", produto1.tipo_de_artigo);
    printf("\tMarca: %s\n", produto1.marca);
    printf("\tModelo: %s\n", produto1.modelo);
    printf("\tPreço: %.2f\n", produto1.preco);
    printf("\tQuantidade disponível: %d\n", produto1.quantidade);

    return 0;
}