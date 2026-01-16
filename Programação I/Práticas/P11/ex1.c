#include <stdio.h>
#include <string.h>

int tamanho(char s[]){
    int i = 0;

    while(s[i] != '\0'){
        i++;
    }

    return i;
}

int main(){
    char texto[1000];
    printf("Digite o texto: ");
    scanf("%[^\n]", texto);     //use getchar() or '%*c' to consume the newline, '*' is used to read but ignore it
    getchar();

    int string = tamanho(texto);

    printf("O tamanho da string é %d.\n", string);

    return 0;
}

/*      USING MALLOC AND FREE FOR INFINITE CHAR
#include <stdio.h>
#include <stdlib.h>

int tamanho(char s[]){
    int i = 0;

    while(s[i] != '\0'){
        i++;
    }

    return i;
}

int main(){
    char *texto;
    printf("Digite o texto: ");
    scanf("%m[^\n]", &texto);
    int string = tamanho(texto);
    printf("O tamanho da string é %d.\n", string);
    free(texto);
    return 0;
}
*/