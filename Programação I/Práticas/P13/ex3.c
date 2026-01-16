#include <stdio.h>

double tempoDecorrido(double velocidade, double distancia){
    if(velocidade <= 0){
        return -1;
    }
    
    double tempo = distancia / velocidade;
    return tempo;
}

int main(){
    double v, d;

    printf("Digite a velocidade em km/h: ");
    scanf("%lf", &v);
    getchar();

    printf("Digite a distância em km: ");
    scanf("%lf", &d);
    getchar();

    double y = tempoDecorrido(v, d);

    if(y = -1){
        printf("A velocidade tem de ser superior a 0.\n");
    
    }else{
        printf("Serão necessárias %.2lf horas para percorrer a distância.\n", y);
    }
    return 0;
}