#include <stdio.h>
#include <stdlib.h>

int main(){
    float a; //salário atual
        printf("Digite o salário do funcionário:");
        scanf("%f", &a);
    
    float b; //percentual de reajuste
        if(a <= 1000){
            b = 0.05;
        }else{
            if(a <= 1800){
                b = 0.04;
            }else{
                if(a <= 2500){
                    b = 0.03;
                }else{
                    b = 0.02;
                }
            }
        }

    float c; //valor de reajuste em euros
        c = a * b;
    
    float d; //salário após o reajuste
        d = a+c;

    printf("O salário atual é igual a: %.2f\n", a);
    printf("O percentual de reajuste é igual a: %.2f\n", b);
    printf("O valor do reajuste em euros é igual a: %.2f\n", c);
    printf("O salário após o reajuste é igual a: %.2f\n", d);
}