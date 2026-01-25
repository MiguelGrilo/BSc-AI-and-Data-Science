#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int conta_alunos(FILE *ficheiro) {
    int contador = 0;
    char nome[50];
    int matricula, nota;

    while (fscanf(ficheiro, "%s %d %d", nome, &matricula, &nota) == 3) {
        contador++;
    
    }
    return contador;
}

int main() {
    char nome[50];
    printf("Digite o nome do ficheiro (por exemplo, notas.txt): ");
    scanf("%s", nome);

    FILE *ficheiro = fopen(nome, "r");

    if (ficheiro == NULL) {
        printf("Erro ao abrir o ficheiro.\n");
        return 1;
    
    }
    int alunos = conta_alunos(ficheiro);

    printf("O número de alunos no ficheiro é: %d\n", alunos);

    fclose(ficheiro);

    return 0;
}