#include <stdio.h>
#include <stdlib.h>

int *vmaior(int *v1, int *v2){
    if(*v1 > *v2){
        return v1;

    }else{
        return v2;

    }
}


int main(){
    int v1, v2;

    printf("Digite o primeiro valor: ");
    scanf("%d", &v1);
    getchar();

    printf("Digite o segundo valor: ");
    scanf("%d", &v2);
    getchar();

    int *endereco_vmaior = vmaior(&v1, &v2);

    printf("O endereço do maior valor é: %p\n", (void *)endereco_vmaior);

    return 0;
}