#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define INICIO 0
#define TOTALCASAS 12
#define NCASAS (TOTALCASAS / 2)
#define CASAS (NCASAS - 1)
#define NFILAS 2
#define FILAS (NFILAS - 1)
#define PEDRAS 48
#define PEDRAS_INICIAIS (PEDRAS / 12)
#define VITORIA 25
#define EMPATE (VITORIA - 1)
#define INVALIDO (-1)


typedef struct{                     //ESTRUTURA DO TABULEIRO
    int casas[NFILAS][NCASAS];      //INSERCAO DAS CASAS
    int depositoA;                  //INSERCAO DO DEPOSITO A
    int depositoB;                  //INSERCAO DO DEPOSITO B

} TABULEIRO;

int modoJOGO(){                             //ESCOLHA DO MODO DE JOGO
    int modo = INVALIDO;                    //INICIAMOS A FUNCAO COM UM VALOR INVALIDO PARA GARANTIR QUE O CODIGO SEJA OBRIGADO A CORRER ESTA FUNCAO E ATRIBUA UM VALOR A MODO
    
    while(modo != 1 && modo != 2){          //MODO 1:   JOGADOR VS JOGADOR          MODO 2:     JOGADOR VS COMPUTADOR
        printf("Escolha o modo de jogo:\n1. Jogador VS Jogador\n2. Jogador VS Computador\nEscolha: ");
        scanf("%d", &modo);

    }
    return modo;                            //APENAS DEVOLVE O VALOR ATRIBUIDO A MODO
}

int iniciarTABULEIRO(TABULEIRO *tabuleiro){                 //VALORES INICIAIS
    tabuleiro->depositoA = 0;                               //VALOR INICIAL DO DEPOSITO A
    tabuleiro->depositoB = 0;                               //VALOR INICIAL DO DEPOSITO B

    for(int i = INICIO; i <= FILAS; i++){
        for(int j = INICIO; j <= CASAS; j++){
            tabuleiro->casas[i][j] = PEDRAS_INICIAIS;       //VALORES INICIAIS DAS CASAS
        
        }
    }
    srand(time(NULL));                                      //INICIA A SEMENTE COM O TEMPO ATUAL(APENAS SERA UTILIZADO NO MODO JOGADOR VS COMPUTADOR)
    return 0;

}

int guardarTABULEIRO(TABULEIRO *tabuleiro){                         //FUNCAO RESPONSÁVEL POR GUARDAR O TABULEIRO ATUAL
    char nomeFICHEIRO[50];
    printf("Digite o nome do ficheiro: ");
    scanf("%s[^\n]", nomeFICHEIRO);                                 //ATRIBUI NOME AO FICHEIRO
    strcat(nomeFICHEIRO, ".txt");                                   //FUNCAO QUE ADICIONA O SUFIXO '.TXT' NO FINAL DO NOME DO FICHEIRO PARA GARANTIR QUE SEJA GUARDADO NO FORMATO DESEJADO

    FILE *ficheiro = fopen(nomeFICHEIRO, "w");                      //ABRIR O FICHEIRO COM A PROPRIEDADE WRITE

    if(ficheiro == NULL){                                           //NO CASO DE NÃO SER POSSIVEL GUARDAR O TABULEIRO APARECERA A INFORMACAO DE QUE OCORREU UM ERRO E O CODIGO SERÁ TERMINADO
        printf("Erro!\n");
        exit(1);

    }else{
        fprintf(ficheiro, "%2d\n", tabuleiro->depositoB);           //IMPRIME O VALOR DO DEPOSITO DO JOGADOR 1/B
        fprintf(ficheiro, "%2d %2d %2d %2d %2d %2d\n", tabuleiro->casas[1][5], tabuleiro->casas[1][4], tabuleiro->casas[1][3], tabuleiro->casas[1][2], tabuleiro->casas[1][1], tabuleiro->casas[1][0]);  //IMPRIME O VALOR DAS CASAS DO JOGADOR 1/B, PELA ORDEM APRESENTADA NO TABULEIRO, OU SEJA, TABULEIRO->CASAS[1][5_4_3_2_1_0]
        
        fprintf(ficheiro, "%2d\n", tabuleiro->depositoA);           //IMPRIME O VALOR DO DEPOSITO DO JOGADOR 0/A
        fprintf(ficheiro, "%2d %2d %2d %2d %2d %2d\n", tabuleiro->casas[0][0], tabuleiro->casas[0][1], tabuleiro->casas[0][2], tabuleiro->casas[0][3], tabuleiro->casas[0][4], tabuleiro->casas[0][5]);  //IMPRIME O VALOR DAS CASAS DO JOGADOR 0/A, PELA ORDEM APRESENTADA NO TABULEIRO, OU SEJA, TABULEIRO->CASAS[0][0_1_2_3_4_5]
        
        fclose(ficheiro);                                           //FECHA O FICHEIRO PREVIAMENTE ABERTO POIS TODOS OS DADOS FORAM GUARDADOS
        
        printf("Tabuleiro guardado em '%s'.\n", nomeFICHEIRO);      //IMPRIME A MENSAGEM DE QUE O TABULEIRO FOI GUARDADO COM SUCESSO E APRESENTA O NOME ATRIBUIDO AO FICHEIRO
        
    }
    return 0;

}

int escolhaVALIDA(TABULEIRO *tabuleiro, int jogadorATUAL, int casaJOGADA){          //FUNCAO PARA VALIDAR OU NAO A ESCOLHA
    int casaATUAL = casaJOGADA - 1;                                                 //'CASAJOGADA' => {1, 2, 3, 4, 5, 6}        'CASAATUAL' => {0, 1, 2, 3, 4, 5} (VALOR A QUE CORREPONDE REALMENTE NOS VETORES) 
    int adversario = (jogadorATUAL == 0) ? 1 : 0;
    int somaADVERSARIO = tabuleiro->casas[adversario][0] + tabuleiro->casas[adversario][1] + tabuleiro->casas[adversario][2] + tabuleiro->casas[adversario][3] + tabuleiro->casas[adversario][4] + tabuleiro->casas[adversario][5];

    if(casaJOGADA == 0){
        return 0;                                                               //AO SER DEVOLVIDO O VALOR '0' NESTA FUNCAO, O CODIGO PROSSEGUIRA NORMALMENTE EM PROL DE GUARDAR O TABULEIRO ATUAL
    
    }else if(casaJOGADA >= 1 && casaJOGADA <= 6){
        if(tabuleiro->casas[jogadorATUAL][casaATUAL] == 0){                     
            return INVALIDO;                                                           //REPETE SE A CASA ESCOLHIDA ESTIVER VAZIA (0 PEDRAS)
        
        }else if((tabuleiro->casas[jogadorATUAL][casaATUAL] == 1) && (tabuleiro->casas[jogadorATUAL][0] > 1|| tabuleiro->casas[jogadorATUAL][1] > 1|| tabuleiro->casas[jogadorATUAL][2] > 1|| tabuleiro->casas[jogadorATUAL][3] > 1|| tabuleiro->casas[jogadorATUAL][4] > 1|| tabuleiro->casas[jogadorATUAL][5] > 1)){
            return INVALIDO;                                                           //REPETE SE A CASA ESCOLHIDA TIVER APENAS 1 PEDRA E EXISTIREM OUTRAS CASAS COM MAIS DE 1 PEDRA
        
        }else if((somaADVERSARIO == 0) && (casaJOGADA + tabuleiro->casas[jogadorATUAL][casaATUAL] < 7) && ((1 + tabuleiro->casas[jogadorATUAL][0] >= 7) || (2 + tabuleiro->casas[jogadorATUAL][1] >= 7) || (3 + tabuleiro->casas[jogadorATUAL][2] >= 7) || (4 + tabuleiro->casas[jogadorATUAL][3] >= 7) || (5 + tabuleiro->casas[jogadorATUAL][4] >= 7) || (6 + tabuleiro->casas[jogadorATUAL][5] >= 7))){
                return INVALIDO;

        }else if((somaADVERSARIO == 0) && ((1 + tabuleiro->casas[jogadorATUAL][0] < 7) || (2 + tabuleiro->casas[jogadorATUAL][1] < 7) || (3 + tabuleiro->casas[jogadorATUAL][2] < 7) || (4 + tabuleiro->casas[jogadorATUAL][3] < 7) || (5 + tabuleiro->casas[jogadorATUAL][4] < 7) || (6 + tabuleiro->casas[jogadorATUAL][5] < 7))){
            return 2;                                                           //TERMINA O JOGO POIS NAO E POSSIVEL PROSSEGUIR COM NENHUMA JOGADA QUE RESPEITE AS REGRAS

        }else{
            return 1;                                                           //AO SER DEVOLVIDO O VALOR '1' NESTA FUNCAO, A JOGADA PROSSEGUIRA NORMALMENTE
        
        }
    }else{
        return INVALIDO;                                                               //AO SER DEVOLVIDO O VALOR 'INVALIDO' NESTA FUNCAO, O CODIGO VAI REPETIR O LOOP RESPONSAVEL POR ACEITAR UM VALOR PARA A 'CASAJOGADA' ATE QUE O VALOR SEJA VALIDO
    
    }
}

int jogadaPlayerA(TABULEIRO *tabuleiro, int jogadorATUAL, int casaJOGADA){     //FUNCAO RESPONSAVEL POR REALIZAR OS MOVIMENTOS DO PLAYER 0/A
    int casaATUAL = casaJOGADA - 1;
    int pedras = tabuleiro->casas[jogadorATUAL][casaATUAL];
    tabuleiro->casas[jogadorATUAL][casaATUAL] = 0;

    int quociente = (casaJOGADA + pedras) / TOTALCASAS;
    int resto = (casaJOGADA + pedras) % TOTALCASAS;
    int voltas = quociente;

    if(quociente == 0){
        for(int i = 1; i <= pedras; i++){
            tabuleiro->casas[jogadorATUAL][casaATUAL + i]++;

        }
    }else if(quociente != 0 && resto == 0){
        for(int i = 1; i < TOTALCASAS - casaATUAL; i++){
            tabuleiro->casas[jogadorATUAL][casaATUAL + i] += voltas;

        }
        for(int i = 0; i < casaATUAL; i++){
            tabuleiro->casas[jogadorATUAL][casaATUAL - i] += (voltas - 1);

        }
    }else if(quociente != 0 && resto != 0){
        if(pedras <= 12){
            for(int i = 1; i < TOTALCASAS - casaATUAL; i++){
                tabuleiro->casas[jogadorATUAL][casaATUAL + i]++;

            }
            for(int i = 1; i <= resto; i++){
                tabuleiro->casas[jogadorATUAL][-1 + i]++;

            }
        }else if(pedras > 12){
            for(int i = 1; i < TOTALCASAS - casaATUAL; i++){
                tabuleiro->casas[jogadorATUAL][casaATUAL + i] += voltas;

            }
            for(int i = 0; i < casaATUAL; i++){
                tabuleiro->casas[jogadorATUAL][casaATUAL - i] += (voltas - 1);

            }
            for(int i = 1; i <= resto + 1; i++){
                tabuleiro->casas[jogadorATUAL][-1 + i]++;

            }
            tabuleiro->casas[jogadorATUAL][casaATUAL]--;

        }
    }
    int casaADVERSARIA = (casaJOGADA + pedras) % (TOTALCASAS + 1);
    int ultimaCASA = NCASAS - (TOTALCASAS - casaADVERSARIA) - 1;
    
    if((casaADVERSARIA >= 7 && casaADVERSARIA <= 12) && (tabuleiro->casas[1][ultimaCASA] >= 2 && tabuleiro->casas[1][ultimaCASA] <= 3)){
        for(int i = ultimaCASA; i >= 0; i--){
            if(tabuleiro->casas[1][i] >= 2 && tabuleiro->casas[1][i] <= 3){                
                tabuleiro->depositoA += tabuleiro->casas[1][i];
                tabuleiro->casas[1][i] = 0;

            }else{
                break;

            }
        }
    }
    return 0;

}

int jogadaPlayerB(TABULEIRO *tabuleiro, int jogadorATUAL, int casaJOGADA){     //FUNCAO RESPONSAVEL POR REALIZAR OS MOVIMENTOS DO PLAYER 1/B
    int casaATUAL = casaJOGADA - 1;
    int pedras = tabuleiro->casas[jogadorATUAL][casaATUAL];
    tabuleiro->casas[jogadorATUAL][casaATUAL] = 0;

    int quociente = (casaJOGADA + NCASAS + pedras) / TOTALCASAS;
    int resto = (casaJOGADA + NCASAS + pedras) % TOTALCASAS;
    int voltas = quociente;
  
    if(quociente == 0){
        for(int i = 1; i <= pedras; i++){
            tabuleiro->casas[jogadorATUAL][casaATUAL + i]++;
        
        }
    }else if(quociente != 0 && resto == 0){
        for(int i = 1; i < (TOTALCASAS - (NCASAS + casaATUAL)); i++){
            tabuleiro->casas[jogadorATUAL][casaATUAL + i] += voltas;
        
        }
        for(int i = 0; i < (NCASAS + casaATUAL); i++){
            tabuleiro->casas[jogadorATUAL][casaATUAL - i] += (voltas - 1);
        
        }
    }else if(quociente != 0 && resto != 0){
        if(pedras <= 12){
            for(int i = 1; i < (TOTALCASAS - (NCASAS + casaATUAL)); i++){
                tabuleiro->casas[jogadorATUAL][casaATUAL + i]++;
            
            }
            jogadorATUAL = (jogadorATUAL == 0) ? 1 : 0;
            for(int i = 1; i <= resto; i++){
                tabuleiro->casas[jogadorATUAL][-1 + i]++;
            
            }
        }else if(pedras > 12){
            for(int i = 1; i < (TOTALCASAS - (NCASAS + casaATUAL)); i++){
                tabuleiro->casas[jogadorATUAL][casaATUAL + i] += voltas;
            
            }
            for(int i = 0; i < (NCASAS + casaATUAL); i++){
                tabuleiro->casas[jogadorATUAL][casaATUAL - i] += (voltas - 1);
            
            }
            jogadorATUAL = (jogadorATUAL == 0) ? 1 : 0;
            for(int i = 1; i <= resto + 1; i++){
                tabuleiro->casas[jogadorATUAL][-1 + i]++;
            
            }
            tabuleiro->casas[jogadorATUAL][casaATUAL]--;

        }
    }
    int casaADVERSARIA = (casaJOGADA + pedras) % (TOTALCASAS + 1);
    int ultimaCASA = NCASAS - (TOTALCASAS - casaADVERSARIA) - 1;
    
    if((casaADVERSARIA >= 7 && casaADVERSARIA <= 12) && (tabuleiro->casas[0][ultimaCASA] >= 2 && tabuleiro->casas[0][ultimaCASA] <= 3)){
        for(int i = ultimaCASA; i >= 0; i--){
            if(tabuleiro->casas[0][i] >= 2 && tabuleiro->casas[0][i] <= 3){                
                tabuleiro->depositoB += tabuleiro->casas[0][i];
                tabuleiro->casas[0][i] = 0;

            }else{
                break;

            }
        }
    }
    return 0;

}

int jogadaAI(TABULEIRO *tabuleiro, int jogadorATUAL, int casaJOGADA){           //FUNCAO RESPONSAVEL POR ESCOLHER A CASA A SER JOGADA PELO COMPUTADOR
    do{
        casaJOGADA = (rand() % NCASAS) + 1;                                     //GERA UM VALOR ALEATORIO E APENAS SERA DEVOLVIDO O RESTO DA SUA DIVISAO POR 6

    }while(escolhaVALIDA(tabuleiro, jogadorATUAL, casaJOGADA) == INVALIDO);

    jogadaPlayerB(tabuleiro, jogadorATUAL, casaJOGADA);
    printf("Turno: Jogador B(Computador)\nJogada efetuada: %d\n", casaJOGADA);
    
    return 0;

}

int jogada(TABULEIRO *tabuleiro, int jogadorATUAL, int casaJOGADA, int modo){       //FUNCAO RESPONSAVEL POR REALIZAR A JOGADA NA GENERALIDADE
    if(modo == 1 || (modo == 2 && jogadorATUAL == 0)){    
        do{
            printf("Turno: Jogador %c\nDigite um valor(1 a 6) para escolher uma casa a jogar:\n*Digite 0 para guardar o tabuleiro atual.\nEscolha: ", (jogadorATUAL == 0) ? 'A' : 'B');
            scanf("%d", &casaJOGADA);

        }while(escolhaVALIDA(tabuleiro, jogadorATUAL, casaJOGADA) == INVALIDO);

        if(escolhaVALIDA(tabuleiro, jogadorATUAL, casaJOGADA) == 0){
            guardarTABULEIRO(tabuleiro);
            exit(1);

        }else if(escolhaVALIDA(tabuleiro, jogadorATUAL, casaJOGADA) == 1 && jogadorATUAL == 0){
            jogadaPlayerA(tabuleiro, jogadorATUAL, casaJOGADA);

        }else if(escolhaVALIDA(tabuleiro, jogadorATUAL, casaJOGADA) == 1 && jogadorATUAL == 1){
            jogadaPlayerB(tabuleiro, jogadorATUAL, casaJOGADA);

        }
    }else if(modo == 2 && jogadorATUAL == 1){
        jogadaAI(tabuleiro, jogadorATUAL, casaJOGADA);

    }
    return 0;

}

int printTABULEIRO(TABULEIRO *tabuleiro){                                                       //VISUALIZACAO DO TABULEIRO
    printf("\t Jogador B\n|---|--|--|--|--|--|--|---|\n");

    printf("|   |");
    for(int j = CASAS; j >= INICIO; j--){                                                       //FILA SUPERIOR (PLAYER 1/B)
        printf("%2d|", tabuleiro->casas[1][j]);
    
    }
    printf("   |\n");

    printf("| %2d|-----------------|%2d |\n", tabuleiro->depositoB, tabuleiro->depositoA);      //DEPOSITOS B---A
    
    printf("|   |");
    for(int j = INICIO; j <= CASAS; j++){                                                       //FILA INFERIOR (PLAYER 0/A)
        printf("%2d|", tabuleiro->casas[0][j]);

    }
    printf("   |\n");
    
    printf("|---|--|--|--|--|--|--|---|\n\t Jogador A\n\n");

    return 0;

}

int terminarJOGO(TABULEIRO *tabuleiro){                                             //FUNCAO QUE DEFINE SE O JOGO TERMINOU/JÁ TEM VENCEDOR
    if(tabuleiro->depositoA >= VITORIA){
        printf("Jogador A venceu!\nCapturou: %d pedras\n", tabuleiro->depositoA);   //VITORIA JOGADOR A
    
    }else if(tabuleiro->depositoB >= VITORIA){
        printf("Jogador B venceu!\nCapturou: %d pedras\n", tabuleiro->depositoB);   //VITORIA JOGADOR B
    
    }else if((tabuleiro->depositoA == EMPATE) && (tabuleiro->depositoB == EMPATE)){
        printf("Empate!\n");                                                        //EMPATE
    
    }else{
        return 0;                                                                   //CONTINUAR JOGO
    
    }
}

int carregarTABULEIRO(TABULEIRO *tabuleiro, char *nomeFICHEIRO){        //FUNCAO RESPONSÁVEL POR CARREGAR UM TABULEIRO GUARDADO PREVIAMENTE
    FILE *ficheiro = fopen(nomeFICHEIRO, "r");                          //ABRE O FICHEIRO A LER COM A PROPRIEDADE READ
    
    if(ficheiro == NULL){                                               //NO CASO DE NÃO SER POSSIVEL CARREGAR O TABULEIRO APARECERA A INFORMACAO DE QUE OCORREU UM ERRO E O CODIGO SERÁ TERMINADO
        printf("Erro ao carregar o ficheiro!\n");
        exit(1);

    }else{
        fscanf(ficheiro, "%2d%*c", &tabuleiro->depositoB);              //SCAN AO VALOR DO DEPOSITO DO PLAYER 1/B
        fscanf(ficheiro, "%2d %2d %2d %2d %2d %2d%*c", &tabuleiro->casas[1][5], &tabuleiro->casas[1][4], &tabuleiro->casas[1][3], &tabuleiro->casas[1][2], &tabuleiro->casas[1][1], &tabuleiro->casas[1][0]);      //SCAN AO VALOR DAS CASAS DO PLAYER 1/B
    
        fscanf(ficheiro, "%2d%*c", &tabuleiro->depositoA);              //SCAN AO VALOR DO DEPOSITO DO PLAYER 0/A
        fscanf(ficheiro, "%2d %2d %2d %2d %2d %2d%*c", &tabuleiro->casas[0][0], &tabuleiro->casas[0][1], &tabuleiro->casas[0][2], &tabuleiro->casas[0][3], &tabuleiro->casas[0][4], &tabuleiro->casas[0][5]);       //SCAN AO VALOR DAS CASAS DO PLAYER 0/A
    
        fclose(ficheiro);

    }
    return 0;

}

int main(int argc, char *argv[]){
    TABULEIRO tabuleiro;

    if(argc == 1){
        iniciarTABULEIRO(&tabuleiro);
    }else if(argc == 2){
        carregarTABULEIRO(&tabuleiro, argv[1]);
    }else{
        printf("Erro!\nUtilize: %s [nomeFicheiro_Tabuleiro]\n", argv[0]);
        return 1;
    }

    int modo = modoJOGO();
    int jogadorATUAL = 0;
    printTABULEIRO(&tabuleiro);

    while(1){
        int casaJOGADA;
        int somaATUAL = tabuleiro.casas[jogadorATUAL][0] + tabuleiro.casas[jogadorATUAL][1] + tabuleiro.casas[jogadorATUAL][2] + tabuleiro.casas[jogadorATUAL][3] + tabuleiro.casas[jogadorATUAL][4] + tabuleiro.casas[jogadorATUAL][5];
    
        if(somaATUAL == 0){
            jogadorATUAL = (jogadorATUAL == 0) ? 1 : 0;
        
        }
        jogada(&tabuleiro, jogadorATUAL, casaJOGADA, modo);
        printTABULEIRO(&tabuleiro);

        jogadorATUAL = (jogadorATUAL == 0) ? 1 : 0;

        if(terminarJOGO(&tabuleiro) != 0 || escolhaVALIDA(&tabuleiro, jogadorATUAL, casaJOGADA) == 2){
            for(int i = INICIO; i <= CASAS; i++){
                tabuleiro.depositoA += tabuleiro.casas[0][i];
                tabuleiro.depositoB += tabuleiro.casas[1][i];
            
            }
            for(int i = INICIO; i <= CASAS; i++){
                tabuleiro.casas[0][i] = 0;
                tabuleiro.casas[1][i] = 0;
            
            }
            break;
        
        }    
    }
    return 0;

}