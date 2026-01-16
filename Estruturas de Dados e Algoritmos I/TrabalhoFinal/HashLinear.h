#include <stdbool.h>

typedef char* LinElementType;

#ifndef _HashLin_H
#define _HashLin_H

//Error messages
#define Error(Str)        FatalError(Str)
#define FatalError(Str)   fprintf(stderr, "%s\n", Str), exit(1)

#define MinTableSize 10

typedef unsigned int Index;
typedef Index LinPosition;

struct LinHashTbl;
typedef struct LinHashTbl *LinHashTable;

bool LinIsPrime(int N);
int LinNextPrime(int N);
LinHashTable LinInitializeTable(int TableSize);
void LinDestroyTable(LinHashTable H);
LinPosition LinProcPos(LinElementType Key, LinHashTable H);
bool LinFind(LinElementType Key, LinHashTable H);
void LinInsert(LinElementType Key, LinHashTable H);
LinElementType LinRetrieve(LinPosition P, LinHashTable H);
LinHashTable LinRehash(LinHashTable H);
void LinPrintTable(LinHashTable H);
float LinLoadFactor(LinHashTable H);

#endif  /* _HashLin_H */