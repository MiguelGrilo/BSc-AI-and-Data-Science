#include "HashQuadratico.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

enum QuadKindOfEntry {QuadLegitimate, QuadEmpty, QuadDeleted};

struct QuadHashEntry{
  QuadElementType Element;
  enum QuadKindOfEntry Info;
};

typedef struct QuadHashEntry QuadCell;

struct QuadHashTbl{   //Cell *TheCells will be an array of HashEntry cells, allocated later
  int Ocupados;
  int TableSize;
  QuadCell *TheCells;
};

typedef struct QuadHashTbl *QuadHashTable;

/* Return next prime; assume N >= 10 */
bool QuadIsPrime( int N ){
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

int QuadNextPrime( int N ){
  while(QuadIsPrime(N) == false){
    N++;
  }
  return N;
}

unsigned int QuadHash(QuadElementType Key, int TableSize){    //Hash para inteiros???
  unsigned int hashValue = 0;
  while(*Key != '\0'){
    hashValue = (hashValue << 5) + *Key++;
  }
  return hashValue % TableSize;
}

QuadHashTable QuadInitializeTable(int TableSize){
  QuadHashTable H;
  int i;

  if(TableSize < MinTableSize){
    Error("Table size too small");
    return NULL;
  }
  
  H=malloc(sizeof(struct QuadHashTbl));   //Aloca a tabela
  if(H == NULL){
    FatalError("Out of space!!!");
  }

  H->TableSize = QuadNextPrime(TableSize);
  
  H->TheCells=malloc(sizeof(QuadCell) * H->TableSize);  //Aloca um array de Cells

  if(H->TheCells==NULL){
    FatalError("Out of space!!!");
  }

  for(i = 0; i < H->TableSize; i++){
    H->TheCells[i].Info = QuadEmpty;
  }
  H->Ocupados = 0;

  return H;
}

QuadPosition QuadProcPos(QuadElementType Key, QuadHashTable H) {
  QuadPosition CurrentPos;

  CurrentPos = QuadHash(Key, H->TableSize);
  int i=0;
  QuadPosition NewP = CurrentPos;
  while(H->TheCells[NewP].Info != QuadEmpty && H->TheCells[NewP].Element != Key){
    i++;
    NewP = CurrentPos;
    NewP += i*i;  // Quadratic probing - Avança para a próxima posição
    if(CurrentPos >= H->TableSize){
      CurrentPos %= H->TableSize;
    }
  }
  CurrentPos = NewP;
  return CurrentPos;
}

bool QuadFind(QuadElementType Key, QuadHashTable H) {
  QuadPosition Pos = QuadProcPos(Key, H);
  return H->TheCells[Pos].Info == QuadLegitimate && strcmp(H->TheCells[Pos].Element, Key) == 0;
}

void QuadInsert(QuadElementType Key, QuadHashTable H){
  if(QuadLoadFactor(H) > 0.35){
    H = QuadRehash(H);
  }
  QuadPosition Pos = QuadProcPos(Key, H);
  if(!QuadFind(Key, H)){
    H->TheCells[Pos].Info = QuadLegitimate;
    H->TheCells[Pos].Element = strdup(Key);   // Usar strdup para duplicar a string
    H->Ocupados++;
  }
}

float QuadLoadFactor(QuadHashTable H){
  return (float)H->Ocupados / H->TableSize;
}

QuadHashTable QuadRehash(QuadHashTable H) {
  int i, OldSize = H->TableSize;
  QuadCell *OldCells = H->TheCells;
  int NewSize = QuadNextPrime(2*OldSize);
  QuadHashTable NewH = QuadInitializeTable(NewSize);  // Cria uma nova tabela de hash com o novo tamanho

  for (i = 0; i < OldSize; i++) {  // Reinsere todos os elementos da tabela antiga na nova tabela
    if (OldCells[i].Info == QuadLegitimate) {
      QuadInsert(OldCells[i].Element, NewH);
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

QuadElementType QuadRetrieve(QuadPosition P, QuadHashTable H){
  return H->TheCells[P].Element;
}

void QuadDestroyTable(QuadHashTable H){
  for(int i = 0; i < H->TableSize; i++){
    if (H->TheCells[i].Info == QuadLegitimate){
      free(H->TheCells[i].Element); //H->TheCells[i].Element = NULL;  //Nullify apontador após liberar
    }
  }
  free(H->TheCells);  //H->TheCells = NULL; //Nullify apontador após liberar
  free(H);  //H = NULL; //Nullify apontador após liberar
}

void QuadPrintTable(QuadHashTable H){
  for(int i = 0; i < H->TableSize; i++){
    if(H->TheCells[i].Info == QuadLegitimate){
      printf("Posição %d: %s\n", i, H->TheCells[i].Element);
    }else{
      printf("Posição %d: Empty\n", i);
    }
  }
}