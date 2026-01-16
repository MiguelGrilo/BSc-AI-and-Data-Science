#include <stdlib.h>
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/Práticas/ap1/fatal.h"
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/Práticas/ap8/abp.h"

struct TreeNode{
  ElementType Element;
  SearchTree  Left;
  SearchTree  Right;
};

SearchTree MakeEmpty(SearchTree T){
  if(T != NULL){
    MakeEmpty(T->Left);
    MakeEmpty(T->Right);
    free(T);
  }

  return NULL;
}


Position Find(ElementType X, SearchTree T){
  if(T == NULL){
    return NULL;
	}else if(X < T->Element){
    return Find(X, T->Left);
	}else if(X > T->Element){
    return Find(X, T->Right);
	}else{
    return T;
	}
}


Position FindMin(SearchTree T){
  if (T == NULL){
    return NULL;
  }else{
    if(T->Left == NULL){
      return T;
		}else{
      return FindMin(T->Left);
		}
	} 
}

Position FindMax(SearchTree T){
  if(T == NULL){
    return NULL;
  } 
  else{
    if(T->Right == NULL){
      return T;
		}else{
      return FindMax(T->Right);
		}
	}
}

SearchTree Insert(ElementType X, SearchTree T){
    T = malloc(sizeof(struct TreeNode));
    if(T == NULL){
      FatalError("Out of space!!!");
    }else{
      T->Element = X;
      T->Left = T->Right = NULL;
    }
    if(X < T->Element){
      T->Left = Insert(X, T->Left);
		}else if(X > T->Element){
      T->Right = Insert(X, T->Right);
		}
    return T;
}

SearchTree Delete(ElementType X, SearchTree T){
  Position Temp;
  if(T == NULL){
    Error("Não encontrado");
	}else if(X < T->Element){
    T->Left = Delete(X, T->Left);
	}else if(X > T->Element){
    T->Right = Delete(X, T->Right);
  }else{
    Temp = T;
    if(T->Left == NULL){
      T = T->Right;
		}else if(T->Right == NULL){
      T = T->Left;
		}
    free(Temp);
  }
  return T;
}

ElementType Retrieve(Position P){
  if(P != NULL){
    return P->Element;
	}else{
    FatalError("Não encontrada");
	}
}