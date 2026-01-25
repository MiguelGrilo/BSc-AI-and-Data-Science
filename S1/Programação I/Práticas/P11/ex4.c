#include <stdio.h>
#include <string.h>

void sub_cadeia(char s[]){
    int length = strlen(s);

    for(int i = 0; i < length && s[i] != '\0'; i++){
        for(int j = 0; j < length - i; j++){
        printf("%c", s[j]);
        }
        printf("\n");
    }

}

int main(){
    char texto[1000];

    printf("Digite o texto: ");
    scanf("%[^\n]", texto);
    getchar();

    sub_cadeia(texto);

    return 0;
}