#include <stdio.h>
#include <stdlib.h>

int main(){
    int a;  //idade
        printf("Digite a idade do nadador:");
        scanf("%d", &a);
    
    if(a >= 5 && a <= 7){   //categoria
        printf("O nadador pertence à categoria: Infantil A\n");
    }else{
        if(a <= 10){
            printf("O nadador pertence à categoria: Infantil B\n");
        }else{
            if(a <= 13){
                printf("O nadador pertence à categoria: Juvenil A\n");
            }else{
                if(a <= 17){
                    printf("O nadador pertence à categoria: Juvenil B\n");
                }else{
                    printf("O nadador pertence à categoria: Sênior\n");
                }
            }
        }
    }
}