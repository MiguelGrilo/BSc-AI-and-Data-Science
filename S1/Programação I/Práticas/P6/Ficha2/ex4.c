#include <stdio.h>
#include <stdlib.h>

int main(){
    int a;  //mês
        printf("Digite o número do mês: ");
        scanf("%d", &a);
        getchar();

    int b;  //ano
        printf("Digite o ano: ");
        scanf("%d", &b);
        getchar();
    
    if(a == 1){
        printf("Janeiro = 31\n");
    }else if(a == 2){
        if((b%100 != 0 && b%4 == 0) || (b%400 == 0)){
            printf("Fevereiro = 29\n");
        }else{
            printf("Fevereiro = 28\n");
        }
    }else if(a == 3){
        printf("Março = 31\n");
    }else if(a == 4){
        printf("Abril = 30\n");
    }else if(a == 5){
        printf("Maio = 31\n");
    }else if(a == 6){
        printf("Junho = 30\n");
    }else if(a == 7){
        printf("Julho = 31\n");
    }else if(a == 8){
        printf("Agosto = 31\n");
    }else if(a == 9){
        printf("Setembro = 30\n");
    }else if(a == 10){
        printf("Outubro = 31\n");
    }else if(a == 11){
        printf("Novembro = 30\n");
    }else if(a == 12){
        printf("Dezembro = 31\n");
    }

    return 0;
}