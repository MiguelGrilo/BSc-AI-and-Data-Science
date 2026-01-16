#include <stdio.h>
#include <stdlib.h>

int main(){
    int a; 
        a = sizeof(int);

    int b;    
        b = sizeof(float);

    int c;    
        c = sizeof(double);
    
    int d;    
        d = sizeof(char);

    printf("int %d\nfloat %d\ndouble %d\nchar %d\n", a, b, c, d);

    return 0;
}