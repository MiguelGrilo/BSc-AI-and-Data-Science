#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/ap1/stackar_folder/stackar_float/functions_stackar_float.h"
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/ap1/stackar_folder/stackar_float/functions_stackar_float.c"
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/ap1/stackar_folder/fatal.h"
#include <stdlib.h>
#include <stdio.h>

int main(){
    int MaxElements;
    printf("Digite o número máximo de elementos para a pilha: ");
    scanf("%d", &MaxElements);
    printf("\n");

    Stack S = CreateStack(MaxElements);

    ElementType elemento;
    printf("Digite os elementos que deseja adicionar à pilha:\n");
    for(int i=0; i < MaxElements; i++){
        printf("Digite o elemento da posição %d: ", i);
        scanf("%f", &elemento);
        Push(elemento, S);
    }

    printf("Conteúdo final da pilha:\n");
    while(!IsEmpty(S)){
        printf("%f\n", Top(S));
        Pop(S);
    }
    
    DisposeStack(S);

    return 0;
}