#include "HashLinear.h"
#include "HashQuadratico.h"

#ifndef _SpellCheck_H
#define _SpellCheck_H

#define TAMANHO_BUFFER 1024

void lerDicionario(LinHashTable dicionario, const char *nomeFicheiro);
void spellCheck(LinHashTable dicionario, QuadHashTable sugestoes, const char *nomeFicheiro);
void geraOutput(LinHashTable dicionario, QuadHashTable sugestoes, const char *nomeFicheiro);

#endif  /* _SpellCheck_H */