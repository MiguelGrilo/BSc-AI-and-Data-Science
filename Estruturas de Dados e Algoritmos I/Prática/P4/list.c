#include "list.h"
#include <stdlib.h>
#include "fatal.h"
#include <stdbool.h>


struct Node{
    ElementType Element;
    Position    Next;
};


List CreateList( List L )
{
    if( L != NULL )
        DeleteList( L );
    else{
        L = malloc( sizeof( struct Node ) );
        if( L == NULL )
            FatalError( "Out of memory!" );
        L->Next = NULL;
    }
    return L;
}



bool IsEmpty( List L ){
    //to be done
    return false;
}

bool IsLast( Position P, List L ){
    //to be done
    return false;
}

Position Find( ElementType X, List L ){
    //to be done
    return NULL;
}


Position FindPrevious( ElementType X, List L ) {
    //to be done
    return NULL;
}


void Insert( ElementType X, List L, Position P ) {
    //to be done
}

void Delete( ElementType X, List L ){
    //to be done
}


void DeleteList( List L ) {
    //to be done
}


Position Header( List L ) {
    //to be done
    return NULL;
}


Position First( List L ) {
    //to be done
    return NULL;
}


Position Advance( Position P ) {
    //to be done
    return NULL;
}


ElementType Retrieve( Position P ) {
    //to be done
    return 0;
}

void PrintList(List L);
//to be done
