#include <stdio.h>
#include <string.h>

struct Artigo {
    char tipo_de_artigo[100];
    char marca[100];
    char modelo[100];
    float preco;
    int quantidade;
};

void alerta_artigos(struct Artigo armazem[], int tamanho) {
    printf("Produtos com quantidade inferior a 10 unidades:\n");

    for (int i = 0; i < tamanho; ++i) {
        if (armazem[i].quantidade < 10) {
            printf("Tipo: %s, Marca: %s, Modelo: %s, Quantidade: %d\n", armazem[i].tipo_de_artigo, armazem[i].marca, armazem[i].modelo, armazem[i].quantidade);
        }
    }
}

int main() {
    // Exemplo de utilização
    struct Artigo armazem[1000];
    int tamanho_do_armazem = 5;  
 
    for(int i = 0; i < tamanho_do_armazem; ++i){
        strcpy(armazem[i].tipo_de_artigo, "TV");
        strcpy(armazem[i].marca, "Samsung");
        strcpy(armazem[i].modelo, "SmarTV Home");
        armazem[i].preco = 100.00;
        armazem[i].quantidade = i * 5;
    }

    alerta_artigos(armazem, tamanho_do_armazem);

    return 0;
}