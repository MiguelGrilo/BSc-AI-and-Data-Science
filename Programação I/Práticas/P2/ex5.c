#include <stdio.h>
#include <stdlib.h>

int main(){
    float a;    //largura
        a = 10;

    float b;    //altura
        b = 3;

    float c;    //área
        c = a * b;

    float d;    //custo da tinta por metro quadrado
        d = 2.34;

    float e;    //custo total
        e = c * d;

    printf("A área total da parede é igual a: %.2f metros quadrados.\nO custo de pintar toda a parede é igual a: %.2f€.\n", c, e);
}