#include <stdio.h>

float triangulo(float l1, float l2, float l3){
    if((l1 + l2 > l3) && (l2 + l3 > l1) && (l1 + l3 > l2)){
        return 1; //true
    }else{
        return 0; //false
    }
}

int main(){
    float lado[3];

    int i;
    for(i = 0; i < 3; i++){
    printf("Digite o valor do lado %d: ", i + 1);
    scanf("%f", &lado[i]);
    }

    float y = triangulo(lado[0], lado[1], lado[2]);

    if(y){
        printf("Estamos na presença de um triângulo.\n");
    }else{
        printf("Não estamos na presença de um triângulo.\n");
    }

    return 0;
}