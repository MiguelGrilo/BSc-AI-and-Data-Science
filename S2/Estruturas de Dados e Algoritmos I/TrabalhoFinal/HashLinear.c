#include "HashLinear.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

enum LinKindOfEntry {LinLegitimate, LinEmpty, LinDeleted};

struct LinHashEntry{
  LinElementType Element;
  enum LinKindOfEntry Info;
};

typedef struct LinHashEntry LinCell;

struct LinHashTbl{   //Cell *TheCells will be an array of HashEntry cells, allocated later
  int Ocupados;
  int TableSize;
  LinCell *TheCells;
};

typedef struct LinHashTbl *LinHashTable;

/* Return next prime; assume N >= 10 */
bool LinIsPrime(int N){
  if(N <= 1)
    return false;
  if(N <= 3)
    return true;
  for(int i= 2; i < N; i++){
    if( N % i == 0 ){
      return false;
    }
  }
  return true;
}

int LinNextPrime(int N){
  while(LinIsPrime(N) == false){
    N++;
  }
  return N;
}

unsigned int LinHash(LinElementType Key, int TableSize){    //Hash para inteiros???
  unsigned int hashValue = 0;
  while(*Key != '\0'){
    hashValue = (hashValue << 5) + *Key++;
  }
  return hashValue % TableSize;
}

LinHashTable LinInitializeTable(int TableSize){
  LinHashTable H;
  int i;

  if(TableSize < MinTableSize){
    Error("Table size too small");
    return NULL;
  }
  
  H=malloc(sizeof(struct LinHashTbl));   //Aloca a tabela
  if(H == NULL){
    FatalError("Out of space!!!");
  }

  H->TableSize = LinNextPrime(TableSize);
  
  H->TheCells=malloc(sizeof(LinCell) * H->TableSize);  //Aloca um array de Cells

  if(H->TheCells==NULL){
    FatalError("Out of space!!!");
  }

  for(i = 0; i < H->TableSize; i++){
    H->TheCells[i].Info = LinEmpty;
  }
  H->Ocupados = 0;

  return H;
}

LinPosition LinProcPos(LinElementType Key, LinHashTable H) {
  LinPosition CurrentPos;

  CurrentPos = LinHash(Key, H->TableSize);
  while(H->TheCells[CurrentPos].Info != LinEmpty && strcmp(H->TheCells[CurrentPos].Element, Key) != 0){
    CurrentPos += 1;  // Linear probing
    if(CurrentPos >= H->TableSize){
      CurrentPos %= H->TableSize;
    }
  }
  return CurrentPos;
}

bool LinFind(LinElementType Key, LinHashTable H) {
  LinPosition Pos = LinProcPos(Key, H);
  return H->TheCells[Pos].Info == LinLegitimate && strcmp(H->TheCells[Pos].Element, Key) == 0;
}

void LinInsert(LinElementType Key, LinHashTable H){
  if(LinLoadFactor(H) > 0.35) {
    H = LinRehash(H);
  }
  LinPosition Pos = LinProcPos(Key, H);
  if(!LinFind(Key, H)){
    H->TheCells[Pos].Info = LinLegitimate;
    H->TheCells[Pos].Element = strdup(Key);   // Usar strdup para duplicar a string
    H->Ocupados++;
  }
}

float LinLoadFactor(LinHashTable H){
  return (float)H->Ocupados / H->TableSize;
}

LinHashTable LinRehash(LinHashTable H) {
  int i, OldSize = H->TableSize;
  LinCell *OldCells = H->TheCells;
  int NewSize = LinNextPrime(2*OldSize);
  LinHashTable NewH = LinInitializeTable(NewSize);  // Cria uma nova tabela de hash com o novo tamanho

  for (i = 0; i < OldSize; i++) {  // Reinsere todos os elementos da tabela antiga na nova tabela
    if (OldCells[i].Info == LinLegitimate) {
      LinInsert(OldCells[i].Element, NewH);
    }
  }
  free(OldCells);  // Libera a memória da tabela antiga
  // Atualiza a variável original para apontar para a nova tabela de hash
  H->TableSize = NewH->TableSize;
  H->TheCells = NewH->TheCells;
  H->Ocupados = NewH->Ocupados;
  //free(NewH);  // Libera a estrutura da nova tabela de hash, mas mantém os dados dela
  return H;  // Retorna a tabela de hash original atualizada com o novo tamanho
}

LinElementType LinRetrieve(LinPosition P, LinHashTable H){
  return H->TheCells[P].Element;
}

void LinDestroyTable(LinHashTable H){
  for(int i = 0; i < H->TableSize; i++){
    if (H->TheCells[i].Info == LinLegitimate){
      free(H->TheCells[i].Element); //H->TheCells[i].Element = NULL;  //Nullify apontador após liberar
    }
  }
  free(H->TheCells);  //H->TheCells = NULL; //Nullify apontador após liberar
  free(H);  //H = NULL; //Nullify apontador após liberar
}

void LinPrintTable(LinHashTable H){
  for(int i = 0; i < H->TableSize; i++){
    if(H->TheCells[i].Info == LinLegitimate){
      printf("Posição %d: %s\n", i, H->TheCells[i].Element);
    }else{
      printf("Posição %d: Empty\n", i);
    }
  }
}