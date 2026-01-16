#include <stdio.h>

float triangulo(float l1, float l2, float l3){
    if((l1 + l2 > l3) && (l2 + l3 > l1) && (l1 + l3 > l2)){
        if(l1 != l2 && l2 != l3){
            return 1;       //triângulo escaleno
        }else if(l1 == l2 && l2 == l3){
            return 3;       //triângulo equilátero
        }else{
            return 2;       //triângulo isósceles
        }
    }else{
        return -1;           //não há triângulo
    }
}

int main(){
    float lado[3];

    int i;
    for(i = 0; i < 3; i++){
    printf("Digite o valor do lado %d: ", i + 1);
    scanf("%f", &lado[i]);
    }

    int y = triangulo(lado[0], lado[1], lado[2]);

    switch(y){
        case -1:
            printf("Não há triângulo.\n");
            break;
        case 1:
            printf("Triângulo escaleno.\n");
            break;
        case 2:
            printf("Triângulo isósceles.\n");
            break;
        case 3:
            printf("Triângulo equilátero.\n");
            break;
    }
    
    return 0;
}