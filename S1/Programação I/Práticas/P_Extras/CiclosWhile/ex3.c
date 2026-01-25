#include <stdio.h>

int main(){
    int limite, contagem;
    
    printf("Digite o limite: ");
    scanf("%d", &limite);
    getchar();

    for(int i = 1; i <= limite; i++){
        printf("%d\n", i);
        contagem++;
    }

    for(int i = limite - 1; i >= 0; i--){
        printf("%d\n", i);
    }

    return 0;
}