#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(){
    double a;    //raio
        printf("Digite o valor do raio: ");
        scanf("%lf", &a);

    double pi;  //valor associado a pi
        pi = 3.1415927;
    
    double b;   //área
        b = pi * pow(a, 2);

    double c;   //perímetro
        c = 2 * pi * a;

    printf("A área da circunferência é igual a %.2lf.\n", b);
    printf("O perímetro da circunferência é igual a %.2lf.\n", c);

    return 0;
}