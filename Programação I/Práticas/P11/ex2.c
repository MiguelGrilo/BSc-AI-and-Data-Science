#include <stdio.h>
#include <string.h>

void whileletras(char s[]){
    int i = 0;
    while(s[i] != '\0'){
        printf("%c\n", s[i]);
        i++;
    }
}

void forletras(char s[]){
    for(int i = 0; s[i] != '\0'; i++){
        printf("%c\n", s[i]);
    }
}

int main(){
    char texto[1000];
    printf("Digite o texto: ");
    scanf("%[^\n]*%*c", texto);

    whileletras(texto);

    printf("\n");

    forletras(texto);

    return 0;
}