#include <stdio.h>
#include <ctype.h>

int vogal(char c){
    c = tolower(c);
    
    if(c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'){
        return 1;   //true
    }else{
        return 0;   //false
    }
}

int main(){
    char carater;

    printf("Digite o caráter: ");
    scanf("%c", &carater);

    int y = vogal(carater);
    if(y){
        printf("'%c' é uma vogal.\n", carater);
    }else{
        printf("'%c' não é uma vogal.\n", carater);
    }

    return 0;
}