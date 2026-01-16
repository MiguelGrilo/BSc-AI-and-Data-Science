#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]){
    int a;
    printf("Digite um número inteiro positivo:");
    scanf("%d", &a);

    if(a <= 0){
        printf("Este não é um número inteiro positivo");
    }else{
        printf("Divisores de %d:\n", a);
    }

    for(int i = 1; i <= a; i++){
        if(a % i == 0){
            printf("%d, ", i);
        }
    }
}