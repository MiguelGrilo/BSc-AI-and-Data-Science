#include <stdio.h>

#define ALUNOS 10
#define TESTES 3

int main(){
    float notas[ALUNOS][TESTES];

    int i, j;

    for(i = 0; i < ALUNOS; i++){
        printf("Aluno %d:\n", i + 1);

        for(j = 0; j < TESTES; j++){
            printf("\tTeste %d: ", j + 1);
            scanf("%f", &notas[i][j]);
            getchar();
        }
    }

    int pior1 = 0, pior2 = 0, pior3 = 0;

    for(i = 0; i < ALUNOS; i++){
        if(notas[i][0] < notas[i][1] && notas[i][0] < notas[i][2]){
            pior1++;
        }else if(notas[i][1] < notas[i][0] && notas[i][1] < notas[i][2]){
            pior2++;
        }else{
            pior3++;
        }
    }

    printf("O número de alunos cuja pior nota foi no teste 1 é: %d\n", pior1);
    printf("O número de alunos cuja pior nota foi no teste 2 é: %d\n", pior2);
    printf("O número de alunos cuja pior nota foi no teste 3 é: %d\n", pior3);

    return 0;
}