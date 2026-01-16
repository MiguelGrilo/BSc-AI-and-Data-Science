#include <stdio.h>
#include <stdlib.h>

int main(){
    int a;
    printf("Digite um número:");
    scanf("%d", &a);

    if (a%2 == 0){
        printf("%d é número par\n", a);
    }else{
        printf("%d é número ímpar\n", a);
    }
    "\n";
}