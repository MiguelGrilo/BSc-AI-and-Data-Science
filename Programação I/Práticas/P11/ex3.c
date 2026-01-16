#include <stdio.h>
#include <string.h>

void letras_inv(char s[]){
    int length = strlen(s);

    int i;
    for(i = length - 1; i >= 0 && s[i] != '\0'; i--){
        printf("%c\n", s[i]);
    }

}

int main(){
    char texto[1000];

    printf("Digite o texto: ");
    scanf("%[^\n]%*c", texto);

    letras_inv(texto);

    return 0;
}