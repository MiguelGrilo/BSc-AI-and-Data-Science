#include <stdio.h>
#include <stdlib.h>

int main(){
    int a;
    printf("Digite o limite de contagem:");
    scanf("%d", &a);

    int b;
    b = 0;

    printf("limite da contagem: %d\n", a);

    for(b ; b <= a; b++){
        printf("valor: %d\n", b);
    }
    for(b = a-1 ; b >= 0; b--){
        printf("valor: %d\n", b);
    }

    return 0;

}