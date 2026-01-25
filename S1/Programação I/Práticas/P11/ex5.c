#include <stdio.h>
#include <string.h>

void espelho(char s[]){
    int length = strlen(s);

    printf("%s", s);

    int i;
    for(i = length - 1; i >= 0 && s[i] != '\0'; i--){
        printf("%c", s[i]);
    }

}

int main(){
    char texto[1000];

    printf("Digite o texto: ");
    scanf("%[^\n]%*c", texto);

    espelho(texto);

    return 0;
}