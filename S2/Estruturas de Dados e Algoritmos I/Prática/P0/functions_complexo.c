#include <stdio.h>
#include <stdlib.h>
#include "functions_complexo.h"

struct ComplexoStruct{
    float real;
    float imaginario;
};

typedef struct ComplexoStruct Complexo;

Complexo CriarComplexo(float r, float i){
    Complexo c;
    c.real = r;
    c.imaginario = i;
    return c;
}

void printComplexo(Complexo c){
    printf("%.2f + %.2fi\n", c.real, c.imaginario);
}

Complexo somaComplexo(Complexo x, Complexo y){
    Complexo resultado;
    resultado.real = x.real + y.real;
    resultado.imaginario = x.imaginario + y.imaginario;
    return resultado;
}

Complexo multComplexo(Complexo x, Complexo y){
    Complexo resultado;
    resultado.real = (x.real * y.real) - (x.imaginario * y.imaginario);
    resultado.imaginario = (y.real * x.imaginario) + (x.real * y.imaginario);
    return resultado;
}

Complexo subComplexo(Complexo x, Complexo y){
    Complexo resultado;
    resultado.real = x.real - y.real;
    resultado.imaginario = x.imaginario - y.imaginario;
    return resultado;
}

Complexo divComplexo(Complexo x, Complexo y){
    Complexo resultado;
    float denominador = y.real * y.real + y.imaginario * y.imaginario;
    resultado.real = (x.real * y.real + x.imaginario * y.imaginario) / denominador;
    resultado.imaginario = (x.imaginario * y.real - x.real * y.imaginario) / denominador;
    return resultado;
}

Complexo conjComplexo(Complexo c){
    Complexo resultado;
    resultado.real = c.real;
    resultado.imaginario = (-1)*c.imaginario;
    return resultado;
}