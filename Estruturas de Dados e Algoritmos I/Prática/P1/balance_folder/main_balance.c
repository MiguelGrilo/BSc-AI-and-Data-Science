#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/ap1/balance_folder/functions_balance.h"
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/ap1/balance_folder/functions_balance.c"
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/ap1/stackar_folder/stackar_char/functions_stackar_char.h"

int main(){
    char exp[100];
        //EXPRESSÃO A AVALIAR :::: "[()] {} {[()()]()}" 
    printf("Digite a expressão a balancear: ");
    fgets(exp, sizeof(exp), stdin);

    int length = strlen(exp);
    if(exp[length-1] == '\n'){
        exp[length-1] = '\0';
    
    }
    if(verifica_balanco(exp) == 0){
        printf("A expresssão '%s' não está balanceada!\n",exp);
    
    }else if(verifica_balanco(exp) == 1){
        printf("A expressão '%s' está balanceada!\n",exp);
    
    }
    return 0;
}