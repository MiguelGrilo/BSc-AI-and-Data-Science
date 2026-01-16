#ifndef complexo_h
#define complexo_h

struct ComplexoStruct;
typedef struct ComplexoStruct Complexo;

Complexo CriarComplexo(float r, float i);
void printComplexo(Complexo c);
Complexo somaComplexo(Complexo x, Complexo y);
Complexo multComplexo(Complexo x, Complexo y);
Complexo subComplexo(Complexo x, Complexo y);
Complexo divComplexo(Complexo x, Complexo y);
Complexo conjComplexo(Complexo c);

#endif /* complexo_h */