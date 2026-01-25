#include "FunAux.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

//Função que retornará o menor entre três números 
int min(int a, int b, int c) {
    int min = a < b ? a : b;    //Compara a e b para armazenar o menor
    return min < c ? min : c;   //Compara o min anterior e c para retornar o menor
}

//Distância de Levenshtein entre duas strings
int levenshteinDistance(const char *s, const char *t) {
    int ls = strlen(s), lt = strlen(t); //Comprimento da string s e da string t
    int d[ls + 1][lt + 1];  //Declara uma matriz para os custos
    
    for (int i = 0; i <= ls; i++) d[i][0] = i;  //Inicializa a primeira coluna da matriz
    for (int j = 0; j <= lt; j++) d[0][j] = j;  //Inicializa a primeira linha da matriz
    
    //Preenche a matriz com os custos 
    for (int i = 1; i <= ls; i++) {
        for (int j = 1; j <= lt; j++) {
            int cost = (s[i - 1] == t[j - 1]) ? 0 : 1;  //Define o custo como 0 se os caracteres forem iguais, caso contrário define-os como 1
            d[i][j] = min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + cost);    //Custo de remoção de caractere em s, inserção de caractere em t e substituição de um caractere de s por um caractere de t
        }
    }
    return d[ls][lt];   //Retorna a Distância de Levenshtein
}

//Função que converte uma string em minúsculas
void toLowerString(char *str) {
    for (int i = 0; i< strlen(str); i++) {  //Itera cada caractere de str
        str[i] = tolower((unsigned char)str[i]);    //Converte o caractere atual para minúscula
    }
}