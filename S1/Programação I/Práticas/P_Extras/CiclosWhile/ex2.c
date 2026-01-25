#include <stdio.h>
#include <math.h>

int main(){
    float cat1, cat2, hip;

    printf("Digite o valor do cateto 1: ");
    scanf("%f", &cat1);
    getchar();

    printf("Digite o valor do cateto 2: ");
    scanf("%f", &cat2);
    getchar();

    hip = sqrt((pow(cat1, 2))+(pow(cat2, 2)));

    printf("O valor da hipotenusa é %.1f.\n", hip);

    return 0;
}