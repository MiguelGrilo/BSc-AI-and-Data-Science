#include <stdio.h>
#include <string.h>

struct Artigo{
    char tipo_de_artigo[100];
    char marca[100];
    char modelo[100];
    float preco;
    int quantidade;
};

int total_artigo(struct Artigo armazem[], int tamanho, char marca[], char modelo[]){
    int total = 0;

    for(int i = 0; i < tamanho; i++){
        if(strcmp(armazem[i].marca, marca) == 0 && (strcmp(armazem[i].modelo, modelo) == 0)){
            total += armazem[i].quantidade;
        }
    }

    return total;
}

int main(){
    struct Artigo armazem[1000];
    int tamanho_armazem = 5;

    for(int i = 0; i < tamanho_armazem; i++){
        strcpy(armazem[i].marca, "marca1");
        strcpy(armazem[i].modelo, "modelo1");
        armazem[i].quantidade = 10;
    }

    char procurar_marca[100] = "marca1";
    char procurar_modelo[100] = "modelo1";

    int total = total_artigo(armazem, tamanho_armazem, procurar_marca, procurar_modelo);

    printf("Existem %d artigos da marca %s e modelo %s.\n", total, procurar_marca, procurar_modelo);

    return 0;
}