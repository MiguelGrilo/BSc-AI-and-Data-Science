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
void infos(FILE *ficheiro, char *aluno[], int nr[], int nota[], int sz){
    char nome[50];
    int matricula, pontuacao;
    for(int i = 0; i < sz; i++){
        if(fscanf(ficheiro, "%s %d %d", nome, &matricula, &pontuacao) == 3){
            aluno[i] = strdup(nome);
            nr[i] = matricula;
            nota[i] = pontuacao;
        }else{
            break;
        }
    }
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
    rewind(ficheiro);
    char *aluno[alunos];
    int nr[alunos];
    int nota[alunos];
    infos(ficheiro, aluno, nr, nota, alunos);
    printf("Informações dos Alunos:\n");
    for(int i = 0; i < alunos; i++){
        printf("Nome: %s\t\tMatrícula: %d\tNota:%d\n", aluno[i], nr[i], nota[i]);
        free(aluno[i]);
    }
    fclose(ficheiro);
    return 0;
}