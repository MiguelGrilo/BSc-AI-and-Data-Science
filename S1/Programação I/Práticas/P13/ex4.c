#include <stdio.h>

float tempoDecorrido(float velocidade, float distancia){
    if(velocidade <= 0){
        printf("A velocidade tem de ser superior a 0.");
        return -1;
    }
    
    float tempo = distancia / velocidade;
    return tempo;
}

float hc(int hp, int mp, float distancia[], float velocidade[], int n){

    int hc = hp;
    int mc = mp;

    for(int i = 0; i < n; i++){
        float tempo = tempoDecorrido(distancia[i], velocidade[i]);
        int tempoEmMin = (int) (tempo * 60.0);

        hc += tempoEmMin / 60.0;
        mc += tempoEmMin % 60;
    }
    printf("A hora de chegada será %02d:%02d\n", hc, mc);
    return hc + mc / 60.0;

}

int main(){
    int hp, mp;

    float distancia[] = {2.0, 6.0, 2.0};
    float velocidade[] = {7.5, 10.9, 7.5};
    printf("Digite a hora de partida(24h): ");
    scanf("%d", &hp);
    getchar();

    printf("Digite o minuto de partida: ");
    scanf("%d", &mp);
    getchar();

    float y = hc(hp, mp, distancia, velocidade, sizeof(distancia) / sizeof(distancia[0]));
    
    return 0;
}