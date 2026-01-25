#include <stdio.h>
#include <stdlib.h>

int main(){
    int a;      //valor a pedir
    printf("Digite um número inteiro positivo: ");
    scanf("%d", &a);
    getchar();

    int b;      //número a multiplicar por a
    b = 1;
    printf("Tabuada de %d:\n", a);
    while(b<=10){
        int c;  //multiplicação
        c = a * b;
        printf("%d*%d=%d\n", a, b, c);
        b = b + 1;
    }
    return 0;
}