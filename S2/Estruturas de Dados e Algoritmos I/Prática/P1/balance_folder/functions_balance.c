#include <stdio.h>
#include <stdlib.h>
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/ap1/balance_folder/functions_balance.h"
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/ap1/stackar_folder/stackar_char/functions_stackar_char.h"
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/ap1/stackar_folder/stackar_char/functions_stackar_char.c"

int ParentesisAbrir(char t){
    if(t == '(' || t == '[' || t == '{'){
        return 1;
    }
    return 0;
}

int ParentesisFechar(char t){
    if(t == ')' || t == ']' || t == '}'){
        return 1;
    }
    return 0;
}

int concordam(char a, char b){
    if((a == '(' && b == ')') || (a == '[' && b == ']') || (a == '{' && b == '}')){
        return 1;
    }
    return 0;
}

int verifica_balanco(char exp[]){
    Stack S = CreateStack(100);

    for(int i=0; exp[i] != '\0'; i++){
        char caracter_atual = exp[i];
        if(ParentesisAbrir(caracter_atual) == 1){
            Push(caracter_atual, S);
        
        }
        if(ParentesisFechar(caracter_atual) == 1){
            if(IsEmpty(S)){
                DisposeStack(S);
                return 0;       //Unbalanced
        
            }else{
                char topo = Top(S);
                if(concordam(topo, caracter_atual) == 1){
                    Pop(S);
                
                }else{
                    DisposeStack(S);
                    return 0;   //Unbalanced
                }
            }
        }
    }
    if(IsEmpty(S)){
        DisposeStack(S);
        return 1;               //Balanced
    }else{
        DisposeStack(S);
        return 0;               //Unbalanced
    }

}