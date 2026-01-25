#include <stdio.h>
#include <stdlib.h>

int sucessor(int n){
    return n+1;
}

int main(){
    int n;
    printf("Digite um número inteiro: ");
    scanf("%d", &n);
    getchar();

    printf("O sucessor de %d é %d.\n", n, sucessor(n));

    return 0;
}