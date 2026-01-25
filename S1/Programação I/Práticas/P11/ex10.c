#include <stdio.h>
#include <string.h>
#include <ctype.h>

int detect(char letra){
    letra = tolower(letra);

    return letra;
}

int ocorrencias3(char letra, char string[], int indice){
    int contagem = 0;

    for(int i = indice; string[i] != '\0'; i++){
        if(detect(string[i]) == detect(letra)){
            contagem++;
        }
    }

    return contagem;
}

int main(){
    char letra, string[1000];
    int indice;
    
    printf("Digite a letra: ");
    scanf("%c", &letra);
    getchar();

    printf("Digite a string: ");
    scanf("%[^\n]", string);
    getchar();

    printf("Digite o indice (começa em 0): ");
    scanf("%d", &indice);
    getchar();

    int y = ocorrencias3(letra, string, indice);

    printf("A partir do indice %d letra %c aparece na string %d vezes.\n", indice, detect(letra), y);

    return 0;
}