#include <stdio.h>
#include <stdlib.h>

int main(){
    float x;
        printf("Digite o valor de x: ");
        scanf("%f", &x);
    
    float y;
        printf("Digite o valor de y: ");
        scanf("%f", &y);

    if(x > 0 && y > 0){
        printf("A coordenada (x, y) = (%.1f, %.1f) pertence ao quadrante 1.\n", x, y);
   
    }else if(x < 0 && y > 0){
        printf("A coordenada (x, y) = (%.1f, %.1f) pertence ao quadrante 2.\n", x, y);
   
    }else if(x < 0 && y < 0){
        printf("A coordenada (x, y) = (%.1f, %.1f) pertence ao quadrante 3.\n", x, y);
   
    }else if(x > 0 && y < 0){
        printf("A coordenada (x, y) = (%.1f, %.1f) pertence ao quadrante 4.\n", x, y);
   

    }else if(x == 0 && y > 0){
        printf("A coordenada (x, y) = (%.1f, %.1f) pertence ao semieixo positivo dos y.\n", x, y);

    }else if(x == 0 && y < 0){
        printf("A coordenada (x, y) = (%.1f, %.1f) pertence ao semieixo negativo dos y.\n", x, y);

    }else if(x > 0 && y == 0){
        printf("A coordenada (x, y) = (%.1f, %.1f) pertence ao semieixo positivo dos x.\n", x, y);

    }else if(x < 0 && y == 0){
        printf("A coordenada (x, y) = (%.1f, %.1f) pertence ao semieixo negativo dos x.\n", x, y);
    }
}