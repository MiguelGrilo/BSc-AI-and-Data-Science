#include <stdio.h>
#include <stdlib.h>

int main(){
    float a;
    printf("Digite o salário do trabalhador:");
    scanf("%f", &a);
    
    float b;
    printf("Digite a prestação:");
    scanf("%f", &b);

    if(b>0.20*a){
        printf("Empréstimo não concedido\n");
    }else{
        printf("Empréstimo concedido\n");
    }
}