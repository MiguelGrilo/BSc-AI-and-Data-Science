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

int maior_nota(int nota[], int sz){
    if(sz <= 0){
        return -1;
    }
    int maior_indice = 0;
    for(int i = 1; i < sz; i++){
        if(nota[i] > nota[maior_indice]){
            maior_indice = i;
        }
    }
    return maior_indice;
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
    fclose(ficheiro);
    printf("Informações dos Alunos:\n");
    for(int i = 0; i < alunos; i++){
        printf("Nome: %s\t\tMatrícula: %d\tNota:%d\n", aluno[i], nr[i], nota[i]);
        //free(aluno[i]);
    }
    int indice_maior_nota = maior_nota(nota, alunos);
    if(indice_maior_nota != -1){
        printf("O aluno com a maior nota é: %s, com nota: %d\n", aluno[indice_maior_nota], nota[indice_maior_nota]);
    }
    return 0;
}