#include <stdio.h>
#include <string.h>
#include <ctype.h>

int detect(char letra){
    letra = tolower(letra);

    return letra;
}

int ocorrencias(char letra, char string[]){
    int contagem = 0;

    for(int i = 0; string[i] != '\0'; i++){
        if(detect(string[i]) == detect(letra)){
            contagem++;
        }
    }

    return contagem;
}

int main(){
    char letra, string[1000];

    printf("Digite a letra: ");
    scanf("%c", &letra);
    getchar();

    printf("Digite a string: ");
    scanf("%[^\n]", string);
    getchar();

    int y = ocorrencias(letra, string);

    printf("A letra %c aparece na string %d vezes.\n", detect(letra), y);

    return 0;
}