#include <stdio.h>

int factorial(int n){
    int total = 1;
    int m = 1;

    while (m <= n){
        total *= m;
        m++;
    }

    return total;
}

int main(){
    int num;
    printf("Digite o número: ");
    scanf("%d", &num);
    getchar();

    int y = factorial(num);

    printf("O facorial de %d! é %d.\n", num, y);

    return 0;
}