#include <stdio.h>
#include <stdlib.h>

int main(){
    float a;    //salário
        printf("Digite o salário: ");
        scanf("%f", &a);
        getchar();

    char b;     //estado civil
        printf("Digite o estado civil (C para casado, S para solteiro): ");
        scanf("%c", &b);
        getchar();

    float c;    //taxa    
        if((b == 'C') || (b == 'c')){
            c = 0.05;
        }else{
            if((b == 'S') || (b == 's')){
                c = 0.10;
            }else{
                printf("Estado civil incorreto.\n");
                return 0;
            }
        }

    float d;    //salário após taxa
        d = a - (a * c);
    
    printf("O salário após taxa é igual a %.2f€.\n", d);

}