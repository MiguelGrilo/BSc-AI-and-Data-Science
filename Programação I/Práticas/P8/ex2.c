#include <stdio.h>
#include <stdlib.h>

float media4(float a, float b, float c, float d){
    return (a + b + c + d)/4;
}

int main(){
    float a, b, c, d;
        printf("Digite os valores: ");
        scanf("%f %f %f %f", &a, &b, &c, &d);
        getchar();

    float y;
        y = media4(a, b, c, d);

    printf("A média é igual a: %.2f.\n", y);

    return 0;
}