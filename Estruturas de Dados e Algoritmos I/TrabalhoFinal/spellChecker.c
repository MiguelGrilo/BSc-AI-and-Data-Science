#include "spellChecker.h"
#include "HashLinear.h"
#include "HashLinear.c"
#include "HashQuadratico.h"
#include "HashQuadratico.c"
#include "FunAux.h"
#include "FunAux.c"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

//Função para ler o dicionário e inserir as palavras na HashLinear
void lerDicionario(LinHashTable dicionario, const char *nomeFicheiro) {
    FILE *ficheiro = fopen(nomeFicheiro, "r");  //Abre o ficheiro do dicionário
    if (ficheiro == NULL) { //Retorna erro caso não seja possível abrir o ficheiro do dicionário
        perror("Erro na abertura do dicionário.");
        return;
    }

    char buffer[TAMANHO_BUFFER];    //Declara o buffer
    while (fgets(buffer, sizeof(buffer), ficheiro)) {   //Lê o ficheiro do dicionário linha a linha
        buffer[strcspn(buffer, "\n")] = '\0';  // Remove o caractere de nova linha
        LinInsert(buffer, dicionario);  //Insere a palavra na HashLinear
    }
    fclose(ficheiro);   //Fecha o ficheiro do dicionário
}

//Função para verificar a ortografia e gerar sugestões
void spellCheck(LinHashTable dicionario, QuadHashTable sugestoes, const char *nomeFicheiro) {
    FILE *ficheiroEntrada = fopen(nomeFicheiro, "r");   //Abre o ficheiro de entrada ( "teste.txt" )
    if (ficheiroEntrada == NULL) {  //Retorna erro caso não seja possível abrir o ficheiro de entrada
        perror("Error na abertura do ficheiro de entrada.");
        return;
    }

    char buffer[TAMANHO_BUFFER];
    while (fgets(buffer, sizeof(buffer), ficheiroEntrada)) {    //Lê o ficheiro de entrada linha a linha
        buffer[strcspn(buffer, "\n")] = '\0';  //Remove o caractere de nova linha

        char *Token = strtok(buffer, " !?,.;:");  //Tokeniza cada linha
        while (Token != NULL) {
            char lower[TAMANHO_BUFFER];
            strcpy(lower, Token);   //Copia o 'Token' atual para a variável 'lower'
            toLowerString(lower);   //Converte 'lower' para minúsculas
            
            if (!LinFind(lower, dicionario)) {  //Verifica se a palavra não se encontra no dicionário                
                if (!QuadFind(lower, sugestoes)) {  //Verifica se a palavra não se encontra nas sugestões
                    QuadInsert(lower, sugestoes);   //Insere a palavra incorreta nas sugestões

                    char Temp[TAMANHO_BUFFER];
                    //Gera sugestões de palavras ao adicionar uma letra em cada posição
                    for(char c = 'a'; c <= 'z'; c++){   //Loop para cada letra do alfabeto
                        for(int i = 0; i <= strlen(lower); i++) {   //Loop para as posições em 'lower' +1, para possibilitar a inserção no final da palavra
                            strncpy(Temp, lower, i);    //Copia os primeiros i caracteres de 'lower' para 'Temp'
                            Temp[i] = c;    //Insere o caractere atual da variável c na posição i de 'Temp'
                            strcpy(Temp + i + 1, lower + i);    //Copia o resto da string 'lower' para o 'Temp'
                            if(LinFind(Temp, dicionario) && !QuadFind(Temp, sugestoes)){    //Se a Temp se encontrar no dicionário e não se encontrar nas sugestões...
                                QuadInsert(Temp, sugestoes);    //Insere a Temp nas sugestões
                            }
                        }
                    }
                    
                    //Gera sugestões de palavras ao remover uma letra em cada posição
                    for(int i = 0; i <= strlen(lower); i++){
                        if(lower[i] == lower[i + 1]){   //Se a letra atual for igual à seguinte, pula
                            continue;
                        }
                        strncpy(Temp, lower, i);    //Copia os primeiros i caracteres de 'lower' para Temp
                        strcpy(Temp + i, lower + i + 1);    //Copia o resto da string 'lower' para Temp, partir de i+1, ou seja, remove o caractere na posição i
                        if(LinFind(Temp, dicionario) && !QuadFind(Temp, sugestoes)){    //Se a Temp se encontrar no dicionário e não se encontrar nas sugestões...
                            QuadInsert(Temp, sugestoes);    //Insere a Temp nas sugestões
                        }
                    }
                        
                    //Gera sugestões de palavras de palavras ao trocar letras adjacentes
                    for(int i = 0; i < strlen(lower) - 1; i++){
                        strcpy(Temp, lower);    //Copia a string 'lower' para Temp
                        char Temp_char = Temp[i];   //Armazena o caractere da posição i em Temp_char
                        //Troca o caractere na posição i com o caractere na posição i+1
                        Temp[i] = Temp[i + 1];
                        Temp[i + 1] = Temp_char;
                        if(LinFind(Temp, dicionario) && !QuadFind(Temp, sugestoes)){    //Se a Temp se encontrar no dicionário e não se encontrar nas sugestões...
                            QuadInsert(Temp, sugestoes);    //Insere a Temp nas sugestões
                        }
                    }
                }
            }
        Token = strtok(NULL, " !?,.;:");    //Próximo token
        }
    }
    fclose(ficheiroEntrada);    //Fecha o ficheiro de entrada
}

//Função que gera o ficheiro de output com o erro e respetivas sugestões
void geraOutput(LinHashTable dicionario, QuadHashTable sugestoes, const char *nomeFicheiro) {
    FILE *ficheiroSaida = fopen(nomeFicheiro, "w"); //Cria o ficheiro de output
    if (ficheiroSaida == NULL) {
        perror("Erro na criação do ficheiro de output.");
        return;
    }
    
    bool printed[sugestoes->TableSize]; //Array que rastreia as palavras já impressas
    memset(printed, false, sizeof(printed));    //Inicializa o array com o valor false

    for (int i = 0; i < sugestoes->TableSize; i++) {
        if (sugestoes->TheCells[i].Info == QuadLegitimate && !printed[i]) {
            if (!LinFind(sugestoes->TheCells[i].Element, dicionario)) {    //Verifica se a palavra não se encontra no dicionário
                char *palavraIncorreta = sugestoes->TheCells[i].Element;    //Palavra incorreta encontrada
                fprintf(ficheiroSaida, "%s -> ", palavraIncorreta);
                printed[i] = true;  //Marca a palavra como já impressa
                int count = 0;

                for (int j = 0; j < sugestoes->TableSize; j++) {    //Imprime 3 sugestões max
                    if (count >= 3) {   //Evita que sejam impressas mais que 3 sugestões, 4 palavras no total, 1 palavra incorreta e 3 sugestões
                        break;
                    }
                    if (sugestoes->TheCells[j].Info == QuadLegitimate && !printed[j] && levenshteinDistance(palavraIncorreta, sugestoes->TheCells[j].Element) <= 2) {
                        if (count > 0) {
                            fprintf(ficheiroSaida, ", ");
                        }
                        fprintf(ficheiroSaida, "%s", sugestoes->TheCells[j].Element);
                        printed[j] = true;  //Marca a palavra como já impressa                 
                        count++;
                    }
                }
                fprintf(ficheiroSaida, "\n");
            }
        }
    }
    fclose(ficheiroSaida);  //Fecha o ficheiro de output
}

int main(int argc, char *argv[]) {
    if(argc < 2){
        fprintf(stderr, "Deve ser utilizado o comando no formato: %s <nome_ficheiro_texto>\n", argv[0]);
        return 1;
    }

    const char *fileDic = "portuguese.txt"; //Nome do ficheiro do dicionário
    //Ficheiro .txt a usar: "teste.txt"
    const char *fileTex = argv[1];  //Nome do ficheiro de entrada a verificar

    LinHashTable dicionario = LinInitializeTable(11);
    QuadHashTable sugestoes = QuadInitializeTable(11);
    
    lerDicionario(dicionario, fileDic);
    spellCheck(dicionario, sugestoes, fileTex);
    geraOutput(dicionario, sugestoes, "sugestoes.txt");

    LinDestroyTable(dicionario);
    QuadDestroyTable(sugestoes);
    return 0;
}