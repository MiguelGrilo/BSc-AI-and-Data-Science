#include <stdio.h>
#include <stdlib.h>

int main(){
    float a;    //frequência 1
    printf("Frequência 1:");
    scanf("%f", &a);
    getchar();

    float b;    //frequência 2
    printf("Frequência 2:");
    scanf("%f", &b);
    getchar();

    float c;    //trabalho prático
    printf("Trabalho Prático:");
    scanf("%f", &c);
    getchar();

    float d;    //média das frequências
    d = (a + b) / 2;

    float e;    // classificação final
    e = 0.7*d + 0.3*c;

    float f;    //número de faltas
    printf("Número de faltas: ");
    scanf("%f", &f);
    getchar();

    if(a>8 && b>8 && c>8){
        if(e>=10 && f<=5){
            printf("O aluno foi aprovado na UC com classificaçaõ final de %.2f.\n", e);
        }else{
        printf("O aluno não foi aprovado na UC\n");
        }
    }else{
        if(f<=5){
            printf("O aluno não obteve os resultados mínimos em pelo menos uma frequência ou no trabalho prático.\n");
        }else{
            printf("O aluno ultrapassou o número máximo de faltas.\n");
        }
    }

}