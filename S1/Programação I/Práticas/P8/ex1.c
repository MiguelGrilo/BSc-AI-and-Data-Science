#include <stdio.h>
#include <stdlib.h>
#include <math.h>

float quadrado(float v){
    return pow(v, 2);
}

float cubo(float v){
    return pow(v, 3);
}

float quadrado_mais_cubo(float v){
    return quadrado(v) + cubo(v);
}

int main(){
    float x;
    printf("Escreva um valor inteiro: ");
    scanf("%f", &x);

    float y;
        y = quadrado_mais_cubo(x);
    
    printf("O valor da adição do quadrado e do cubo de %.0f é: %.2f\n", x, y);

    return 0;
}

/*
to run file need to run this in the terminal:
gcc -o ex1 ex1.c -lm
./ex1 -lm
*/