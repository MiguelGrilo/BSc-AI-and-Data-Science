#include <stdbool.h>
typedef char* QuadElementType;

#ifndef _HashQuad_H
#define _HashQuad_H

//Error messages
#define Error(Str)        FatalError(Str)
#define FatalError(Str)   fprintf(stderr, "%s\n", Str), exit(1)

#define MinTableSize 10

typedef unsigned int Index;
typedef Index QuadPosition;

struct QuadHashTbl;
typedef struct QuadHashTbl *QuadHashTable;

bool QuadIsPrime(int N);
int QuadNextPrime(int N);
QuadHashTable QuadInitializeTable(int TableSize);
void QuadDestroyTable(QuadHashTable H);
QuadPosition QuadProcPos(QuadElementType Key, QuadHashTable H);
bool QuadFind(QuadElementType Key, QuadHashTable H);
void QuadInsert(QuadElementType Key, QuadHashTable H);
QuadElementType QuadRetrieve(QuadPosition P, QuadHashTable H);
QuadHashTable QuadRehash(QuadHashTable H);
void QuadPrintTable(QuadHashTable H);
float QuadLoadFactor(QuadHashTable H);

#endif  /* _HashQuad_H */