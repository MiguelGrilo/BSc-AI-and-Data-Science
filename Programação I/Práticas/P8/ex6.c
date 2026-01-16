#include <stdio.h>
#include <stdlib.h>

float velocidade_final(float v0, float a, float t){
    return v0 + a * t;
}

int main(){
    float v0;
    printf("Digite o valor da velocidade inicial: ");
    scanf("%f", &v0);
    getchar();

    float a;
    printf("Digite o valor da aceleração: ");
    scanf("%f", &a);
    getchar();

    float t;
    printf("Digite o tempo em segundos: ");
    scanf("%f", &t);
    getchar();

    float velocidade = velocidade_final(v0, a, t);

    printf("A velocidade é %.2f\n", velocidade);

    return 0;
}