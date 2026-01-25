#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(){
    float a;  //velocidade
    printf("Velocidade: ");
    scanf("%f", &a);

    float b;    //unidade
    printf("Unidades (1 para km/h e 2 para mph): ");
    scanf("%f", &b);

    float c, d; //c = km    e   d = milha
    c = 1;
    d = 0.621371192237 * c;

    float e;    //distância de travagem
    if(b == 1){
        e = (pow((a*c / 10), 2)* 1/2);
    }
    if(b == 2){
        e = (pow((a*d / 10), 2)* 1/2);
    }

    printf("Distância de travagem = %.1fm.\n", e);
}