#include <stdio.h>
#include <string.h>
#include <ctype.h>

int vogal(char c){
    c = tolower(c);
    
    if(c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'){
        return 1;
    }else{
        return 0;
    }
}

int conta_vogais(char s[]){
    int contagem = 0; 
    
    for(int i = 0; s[i] != '\0'; i++){
        if(vogal(s[i]) == 1){
            contagem++;
        }
    }

    return contagem;
}

int main(){
    char string[1000];

    int i;
    printf("Digite a string: ");
    scanf("%[^\n]%*c", string);

    int y = conta_vogais(string);

    printf("A string tem %d vogais.\n", y);

    return 0;
}