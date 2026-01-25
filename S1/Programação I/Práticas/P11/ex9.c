#include <stdio.h>
#include <string.h>
#include <ctype.h>

int ocorrencias2(char letra, char string[], int indice){
    int contagem = 0;

    for(int i = indice; string[i] != '\0'; i++){
        if(string[i] == letra){
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

    printf("Digite o indice: ");
    scanf("%d", &indice);
    getchar();

    int y = ocorrencias2(letra, string, indice);

    printf("A partir do indice %d letra %c aparece na string %d vezes.\n", indice, letra, y);

    return 0;
}