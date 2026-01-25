#include <stdio.h>
#include <stdlib.h>

int main(){
    int valor;
    int total = 0;

    while(1){
        printf("valor: ");
        scanf("%d", &valor);
        
        if(valor == 0){
            break;
        }
    
        total += valor;
    }

    printf("A soma dos vários valores é: %d", total);

    return 0;

}