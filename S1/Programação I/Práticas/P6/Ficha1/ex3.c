#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(){
    float pi;   //valor definido para o pi
        pi = 3.14;

    float a;    //raio da esfera
        printf("Digite o raio da esfera: ");
        scanf("%f", &a);
    
    float b;    //volume da esfera
        b = (4.0/3.0) * pi * (pow(a, 3));

    printf("O volume da esfera é igual a %.2f.\n", b);    
}