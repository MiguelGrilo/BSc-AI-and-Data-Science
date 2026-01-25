#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/ap1/stackar_folder/fatal.h"
#include "/home/miguelgrilo/UNI-L58387-IACD/S2-EDA1/ap1/stackar_folder/stackar_char/functions_stackar_char.h"
#include <stdlib.h>
#include <stdio.h>

#define EmptyTOS ( -1 )
#define MinStackSize ( 5 )

struct StackRecord{
    int Capacity;
    int TopOfStack;
    ElementType *Array;
};

Stack CreateStack(int MaxElements){
    Stack S;
	if(MaxElements < MinStackSize){
		Error("Stack size is too small");
	}else{
		S = malloc(sizeof(struct StackRecord));
	}

	if(S == NULL){
		FatalError("Out of space!!!");
	}else{
		S->Array = malloc(sizeof(ElementType)*MaxElements);
	}

	if(S->Array == NULL){
		FatalError("Out of space!!!");
	}else{
		S->Capacity = MaxElements;
	}

	MakeEmpty(S);
	return S;
}

void DisposeStack(Stack S){
    if(S != NULL){
        free(S->Array);
        free(S);
    }
}

int IsEmpty(Stack S){
	return S->TopOfStack == EmptyTOS;
}

int IsFull(Stack S){
	return S->TopOfStack == S->Capacity-1; 
}

void MakeEmpty(Stack S){
	S->TopOfStack = EmptyTOS;
}

void Push(ElementType X, Stack S){
	if(IsFull(S)){
		Error("Full Stack");
	}else{
		S->Array[++S->TopOfStack] = X;
	}
}

ElementType Top(Stack S){
	if(!IsEmpty(S)){
		return S->Array[S->TopOfStack];
	}else{
		Error("Empty Stack");
	}
	return 0;
}

ElementType Pop(Stack S){
	if(!IsEmpty(S)){
		return S->Array[S->TopOfStack--];
	}else{
		Error("Empty Stack");
	}
	return 0;
}