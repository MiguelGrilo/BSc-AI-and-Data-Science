#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(){
    float x;
        x = 2.7;
    
    float y;
        y = 3;

    float a;
        a = x * (pow(y, 2));
    
    printf("O produto de x pelo quadrado de y é igual a %.2f\n", a);
}