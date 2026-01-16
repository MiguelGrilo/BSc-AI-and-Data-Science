#include <stdio.h>
#include <stdlib.h>
#include "functions_complexo.h"
#include "functions_complexo.c"

int main(){
    float r1, i1, r2, i2;
   
    printf("Insira a parte real do primeiro número complexo: ");
    scanf("%f", &r1);

    printf("Insira a parte imaginária do primeiro número complexo: ");
    scanf("%f", &i1);
    
    printf("Insira a parte real do segundo número complexo: ");
    scanf("%f", &r2);
    
    printf("Insira a parte imaginária do segundo número complexo: ");
    scanf("%f", &i2);

    Complexo c1=CriarComplexo(r1, i1);
    Complexo c2=CriarComplexo(r2, i2);

    printf("Primeiro número complexo: ");
    printComplexo(c1);
    printf("Segundo número complexo: ");
    printComplexo(c2);

    Complexo soma = somaComplexo(c1, c2);
    Complexo produto = multComplexo(c1, c2);
    Complexo diferenca = subComplexo(c1, c2);
    Complexo quociente = divComplexo(c1, c2);
    Complexo conjugado1 = conjComplexo(c1);
    Complexo conjugado2 = conjComplexo(c2);

    printf("\nSoma: ");
    printComplexo(soma);
    printf("Produto: ");
    printComplexo(produto);
    printf("Diferença: ");
    printComplexo(diferenca);
    printf("Quociente: ");
    printComplexo(quociente);
    printf("Conjugado do primeiro número complexo: ");
    printComplexo(conjugado1);
    printf("Conjugado do segundo número complexo: ");
    printComplexo(conjugado2);

    return 0;
}