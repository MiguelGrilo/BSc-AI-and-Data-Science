#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]){
    char a[100]; //nome
    printf("Digite o seu nome: ");
    scanf("%99[^\n]s", a);
    getchar();


    char b[100]; //curso
    printf("Digite o seu curso: ");
    scanf("%99[^\n]s", b);
    getchar();

    printf("Meu nome é %s. Sou aluno do curso de licenciatura em %s.\n", a, b);
}