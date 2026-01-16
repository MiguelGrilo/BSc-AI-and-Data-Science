#include <stdio.h>
#include <stdlib.h>

int main(){
    char a; //operador
        printf("Digite o operador(+, -, /, *): ");
        scanf("%c", &a);
    
    float b;    //
        printf("Digite o número 1: ");
        scanf("%f", &b);

    float c;    //
        printf("Digite o número 2: ");
        scanf("%f", &c);

    float d;    //resultado
        if(a == '+'){
            d = b + c;
            printf("O resultado da expressão %.2f %c %.2f é %.2f.\n", b, a, c, d);

        }else if(a == '-'){
            d = b - c;
            printf("O resultado da expressão %.2f %c %.2f é %.2f.\n", b, a, c, d);
        
        }else if(a == '*'){
            d = b * c;
            printf("O resultado da expressão %.2f %c %.2f é %.2f.\n", b, a, c, d);
        
        }else if(a == '/'){
            if(c != 0){
                d = b / c;
                printf("O resultado da expressão %.2f %c %.2f é %.2f.\n", b, a, c, d);
            }else if(c == 0){
                printf("Erro: Divisão por 0.\n");
            }
        }
}