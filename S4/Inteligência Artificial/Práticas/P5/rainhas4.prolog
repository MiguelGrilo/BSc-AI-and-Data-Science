%rainhas 4 num tabuleiro de 4x4
estado_inicial([(1,1,b),(1,2,b), (1,3,b),  (1,4,b),
	(2,1,b),(2,2,b), (2,3,b),  (2,4,b),
	(3,1,b),(3,2,b), (3,3,b),  (3,4,b),
	(4,1,b),(4,2,b), (4,3,b),  (4,4,b)]).

/*
 estado_inicial([(1,1,r),(1,2,b), (1,3,b),  (1,4,b),
	(2,1,b),(2,2,r), (2,3,b),  (2,4,b),
	(3,1,b),(3,2,b), (3,3,b),  (3,4,b),
	(4,1,b),(4,2,b), (4,3,b),  (4,4,b)
	]).*/
  estado_final(L):- conta4(L,0,N), N =4.

  conta4([],N,N).
  conta4(_L,N,N):- N=4,!.
  conta4([(_,_,r)|R],N,M):- M1 is N+1, conta4(R,M1,M).
  conta4([(_,_,b)|R],N,M):-  conta4(R,N,M).

  op(E,o(X,Y),E1,1):- member((X,Y,b),E),
    substitui((X,Y,b),E,E1),
    ve_restricao(E1).

substitui(_, [],[]). 
substitui((X,Y,b), [(X,Y,b)|R],[(X,Y,r)|S]):-!, substitui((X,Y,b),R,S).
substitui((X,Y,b), [(X1,Y1,A)|R],[(X1,Y1,A)|S]):- substitui((X,Y,b),R,S).

%c(i) <> c(j) e |i-j| <> c(i)-c(j)  

ve_restricao(Afect):- \+ (member((Ix,Iy,r), Afect),
	member((Jx,Jy,r),Afect),
	(Ix,Iy) \= (Jx,Jy),
    (Ix=Jx;Iy=Jy; modDif(Ix,Jx,D1),
	modDif(Iy,Jy,D2), D1=D2)).

modDif(I,J,D):- I>J, D is I-J.
modDif(I,J,D):- I =< J, D is J-I.
  
  
  
