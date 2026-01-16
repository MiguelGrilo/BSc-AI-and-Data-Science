#include <stdio.h>

int conta_palavras(char string[]){
    int contagem = 0;
    int palavra = 0;

    for(int i = 0; string[i] != '\0'; i++){
        if(string[i] == ' ' || string[i] == ','){
            palavra = 0;    //false
        }else{
            if(palavra == 0){
                contagem++;
                palavra = 1;
            }
        }
    }

    return contagem;
}

int main(){
    char string[1000];

    printf("Digite o texto: ");
    fgets(string, sizeof(string), stdin);

    int y = conta_palavras(string);
    
    printf("A string tem %d palavras.\n", y);

    return 0;
}