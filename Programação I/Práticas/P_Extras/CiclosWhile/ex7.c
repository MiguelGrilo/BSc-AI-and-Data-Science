#include <stdio.h>

int main(){
    float litros, tipo, dia, total;

    float gasoleo = 1.711;
    float gasolina95 = 1.798;
    float gasolina98 = 1.838;

    printf("Quantos litros? ");
    scanf("%f", &litros);
    getchar();

    printf("Qual o combustível (1:gasoleo, 2:gasolina95, 3:gasolina98)? ");
    scanf("%f", &tipo);
    getchar();

    printf("Qual o dia do abastecimento? ");
    scanf("%f", &dia);
    getchar();
     
    if(tipo == 1){
        tipo = gasoleo;
    }else if(tipo == 2){
        tipo = gasolina95;
    }else if(tipo == 3){
        tipo = gasolina98;
    }

    if(dia >= 15){
        gasolina95 = 1.819;
        gasoleo = 1.734;
    }

    total = litros * tipo;

    printf("O custo é %.2f.\n", total);

    return 0;
}