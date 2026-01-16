#include <stdio.h>
#include <stdlib.h>

int main(){
    int valores[10];
    
    int i;
    
    for(i=0; i<=9; i++){
        printf("Digite o número inteiro: ");
        scanf("%d", &valores[i]);
    }

    for(i=9; i>=0; i--){            
        printf("%d\t", valores[i]);
    }
    
    return 0;
}