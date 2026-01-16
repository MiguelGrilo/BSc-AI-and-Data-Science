#include <stdio.h>
#include <stdlib.h>

int main(){
    float x, y;

    printf("Digite o número 1: ");
    scanf("%f", &x);
    getchar();

    printf("Digite o número 2: ");
    scanf("%f", &y);
    getchar();

    while(1){
        if(x > y){
            printf("1\n");
            break;
        }else if(x == y){
            printf("0\n");
            break;
        }else{
            printf("-1\n");
            break;
        }
    }

    return 0;
}