#include <stdio.h>
#include <math.h>

int main(){
    float a; //número
        printf("Digite um número:");
        scanf("%f", &a);
    
    if(a != 0){
        float b = pow(a, 2); //quadrado
        printf("O quadadro de %.2f é igual a: %.2f\n", a, b);

        float c = sqrt(a); //raiz
        printf("A raiz de %.2f é igual a: %.2f\n", a, c);
    }else{
        printf("O número precisa ser diferente de 0");
    }
}