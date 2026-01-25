#include <stdio.h>
#include <math.h>

int main() {
    double n, x0, raiz, epsilon = 0.0001;

    printf("Qual o valor de n? ");
    scanf("%lf", &n);

    printf("Qual o valor da estimativa inicial? ");
    scanf("%lf", &x0);

    while (1) {
        raiz = 0.5 * (x0 + n / x0);

        if (fabs(raiz - x0) < epsilon) {
            break;
        }

        x0 = raiz;
    }
    printf("raiz=%f\n", raiz);

    return 0;
}