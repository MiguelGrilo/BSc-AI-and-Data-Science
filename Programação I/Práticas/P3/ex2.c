#include <stdio.h>
#include <stdlib.h>

int main(){
    int a, b, c;    //valores
    scanf("%d" "%d" "%d", &a, &b, &c);

    int d = c;  //número do meio
    if((b < a && a < c)||(c < a && a < b)){
        d = a;
    }else if((a < b && b < c)||(c < b && b < a)){
        d = b;
    }

    printf("O valor do meio é %d.\n", d);
}