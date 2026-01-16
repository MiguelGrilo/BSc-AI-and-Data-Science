#include <stdio.h>
#include <stdlib.h>

#define MAX 1024

int main(){
    FILE *ficheiro_original, *ficheiro_copia;
    char nome_original[100], nome_copia[100];
    char buffer[MAX];
    size_t bytes_lidos;

    printf("Digite o nome do ficheiro original: ");
    scanf("%s", nome_original);

    ficheiro_original = fopen(nome_original, "r");
    if(ficheiro_original == NULL){
        perror("Erro a abrir o ficheiro!");
        exit(1);

    }
    printf("Digite o nome do novo ficheiro: ");
    scanf("%s", nome_copia);

    ficheiro_copia = fopen(nome_copia, "w");
    if (ficheiro_copia == NULL) {
        perror("Erro ao criar ficheiro.");
        fclose(ficheiro_original);
        exit(1);

    }
    while((bytes_lidos = fread(buffer, 1, sizeof(buffer), ficheiro_original)) > 0){
        fwrite(buffer, 1, bytes_lidos, ficheiro_copia);
    
    }
    fclose(ficheiro_original);
    fclose(ficheiro_copia);

    printf("Ficheiro copiado com sucesso.\n");

    return 0;

}