#include <stdlib.h>
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/Práticas/ap1/fatal.h"
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/Práticas/ap7/btree.h"
#include <limits.h> 

struct TreeNode{
  ElementType Element;
  BTree  Left;
  BTree  Right;
};

BTree MakeEmpty(BTree T){             
  if(T != NULL){
    MakeEmpty(T->Left);   //Chama-se recursivamente para os nós à esquerda e à direita
    MakeEmpty(T->Right);
    free(T);    //Libera a memória de T
  }
  return NULL;    //Retorna NULL indicando que a arvore está vazia
}

BTree SetTree(ElementType X, BTree Left, BTree Right){    //cria um novo nó de árvore com um elemento específico  
  BTree T = (BTree)malloc(sizeof(struct TreeNode));
  if(T == NULL){
    FatalError("Out of space");
  }else{
    T->Element = X;
    T->Left = Left;
    T->Right = Right;
  }
  return T;
}

Position Find(ElementType X, BTree T){
    if(T == NULL){    //Caso não encontre o elemento
      return NULL;
    }else if(X < T->Element){   //Se X for menor que o elemento atual, a função chama-se recursivamente no nó à esquerda de T
      return Find(X, T->Left);
    }else if(X > T->Element){    //Se X for maior que o elemento atual, a função chama-se recursivamente no nó à direita de T
      return Find(X, T->Right);                                       
    }else{    //Se X for igual ao elemento do nó atual, retorna a posição desse nó na árvore
      return T;                                   
    }
}

ElementType Retrieve(Position P){
  if(P != NULL){                
    return P->Element;    //Retorna o elemento da posição P
  }else{
    FatalError("Não encontrada");
  }
}

void PrintInOrder(BTree T){
  if(T != NULL){
    PrintInOrder(T->Left);
    printf("%d ", T->Element);
    PrintInOrder(T->Right);
  }
}

void PrintPreOrder(BTree T){
  if(T != NULL){
    printf("%d ", T->Element);
    PrintPreOrder(T->Left);
    PrintPreOrder(T->Right);
  }
}

void PrintPosOrder(BTree T){
  if(T != NULL){
    PrintPosOrder(T->Left);
    PrintPosOrder(T->Right);
    printf("%d ", T->Element);
  }
}

int max(int a, int b){
  return (a > b) ? a : b;
}
int FindMax(BTree T){
  if (T == NULL){
    return INT_MIN; // Retorna o menor valor possível se a árvore estiver vazia
  }
  int maxLeft = FindMax(T->Left);
  int maxRight = FindMax(T->Right);
  
  return max(T->Element, max(maxLeft, maxRight));
}

int min(int a, int b){
  return (a < b) ? a : b;
}
int FindMin(BTree T){
  if (T == NULL) {
    return INT_MAX; // Retorna o maior valor possível se a árvore estiver vazia
  }
  int minLeft = FindMin(T->Left);
  int minRight = FindMin(T->Right);
  return min(T->Element, min(minLeft, minRight));
}