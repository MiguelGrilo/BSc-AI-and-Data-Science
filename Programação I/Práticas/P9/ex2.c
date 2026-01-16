#include <stdio.h>
#include <math.h>
float calcularIMC(float peso, float altura){
    float IMC = peso / pow(altura, 2);
    printf("IMC: %.2f.\n", IMC);
    return IMC;
}
float exibirClassificacao(float calcularIMC){
    float IMC;
    if(IMC < 18.5){
        printf("Pertence à categoria: Abaixo do Peso.\n");
    }else{
        if(IMC < 25){
            printf("Pertence à categoria: Peso Normal\n");
        }else{
            if(IMC < 30){
                printf("Pertence à categoria: Sobrepeso\n");
            }else{
                if(IMC < 35){
                    printf("Pertence à categoria: Obesidade (Grau I)\n");
                }else{
                    if(IMC < 40){
                        printf("Pertence à categoria: Obesidade (Grau II)\n");
                    }else{
                        if(IMC >= 40){
                            printf("Pertence à categoria: Obesidade (Grau III)\n");
                        }
                    }
                }
            }
        }
    }
}
int main(){
    float altura;
        printf("Digite a altura em metros: ");
        scanf("%f", &altura);
        getchar();
    
    float peso;
        printf("Digite o peso em quilogramas: ");
        scanf("%f", &peso);
        getchar();

    float IMC = calcularIMC(peso, altura);
    exibirClassificacao(IMC);
    return 0;
}