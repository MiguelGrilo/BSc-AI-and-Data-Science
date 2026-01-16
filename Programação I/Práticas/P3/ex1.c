#include <stdio.h>
#include <stdlib.h>

int main(){
    int a, b, c;    //valores
    scanf("%d" "%d" "%d", &a, &b, &c);

    int d = a;  //minimo (a foi definido como minimo)
    if(b<d){
        d = b;
    }
    if(c<d){
        d = c;
    }

    printf("O valor mínimo é %d\n", d);
}