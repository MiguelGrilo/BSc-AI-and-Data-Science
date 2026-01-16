%Estado_Inicial
%estado_inicial(e([v,v,v,v,v,v,v,v,v],x)).
inv(x,y).
inv(y,x).

%Estados_terminais
terminal(e(L,_)) :- linha(e(L,_)).
terminal(e(L,_)) :- coluna(e(L,_)).
terminal(e(L,_)) :- diagonal(e(L,_)).
%Linhas
linha(e([ s, o, s,_,_,_,_,_,_ ],_)).
linha(e([_,_,_, s, o, s,_,_,_ ],_)).
linha(e([_,_,_,_,_,_, s, o, s ],_)).
%Colunas
coluna(e([ s,_,_, o,_,_, s,_,_ ],_)).
coluna(e([_, s,_,_, o,_,_, s,_ ],_)).
coluna(e([_,_, s,_,_, o,_,_, s ],_)).
%Diagonal
diagonal(e([ s,_,_,_, o,_,_,_, s ],_)).
diagonal(e([_,_, s,_, o,_, s,_,_ ],_)).

%Funcao_utilidade
valor(e(Tabuleiro, _), 0, _) :- %Empate
  \+ terminal(e(Tabuleiro,_)),
  \+ member(v, Tabuleiro).
valor(e(Tabuleiro, _),V,P):- 
  terminal(e(Tabuleiro,_)),
  X is P mod 2,
  (X == 1 , V =1; %Ganha
  X == 0, V = -1). %Perde

%Operadores
%op(estado, jogada, jogador, novo_estado)
op1(e(Tabuleiro, Jogador), joga(Posicao, Letra), e(NovoTabuleiro, ProximoJogador)) :-
  member(Letra, [ s, o]),
  jogada(e(Tabuleiro, Jogador), (Posicao, Letra) , e(NovoTabuleiro, Jogador)),
  inv(Jogador, ProximoJogador).                     

jogada(e([v,C2,C3,C4,C5,C6,C7,C8,C9],_), (1,X), e([X,C2,C3,C4,C5,C6,C7,C8,C9],_)).
jogada(e([C1,v,C3,C4,C5,C6,C7,C8,C9],_), (2,X), e([C1,X,C3,C4,C5,C6,C7,C8,C9],_)).
jogada(e([C1,C2,v,C4,C5,C6,C7,C8,C9],_), (3,X), e([C1,C2,X,C4,C5,C6,C7,C8,C9],_)).
jogada(e([C1,C2,C3,v,C5,C6,C7,C8,C9],_), (4,X), e([C1,C2,C3,X,C5,C6,C7,C8,C9],_)).
jogada(e([C1,C2,C3,C4,v,C6,C7,C8,C9],_), (5,X), e([C1,C2,C3,C4,X,C6,C7,C8,C9],_)).
jogada(e([C1,C2,C3,C4,C5,v,C7,C8,C9],_), (6,X), e([C1,C2,C3,C4,C5,X,C7,C8,C9],_)).
jogada(e([C1,C2,C3,C4,C5,C6,v,C8,C9],_), (7,X), e([C1,C2,C3,C4,C5,C6,X,C8,C9],_)).
jogada(e([C1,C2,C3,C4,C5,C6,C7,v,C9],_), (8,X), e([C1,C2,C3,C4,C5,C6,C7,X,C9],_)).
jogada(e([C1,C2,C3,C4,C5,C6,C7,C8,v],_), (9,X), e([C1,C2,C3,C4,C5,C6,C7,C8,X],_)).

estado_inicial(e([s,v,v,v,v,s,v,v,o],x)). %1
%estado_inicial(e([s,v,v,v,v,s,v,o,o],y)). %2
%estado_inicial(e([s,v,v,v,v,s,o,o,o],x)). %3
%estado_inicial(e([s,v,v,v,o,s,o,o,o],y)). %4
%estado_inicial(e([s,v,v,o,o,s,o,o,o],x)). %5
%estado_inicial(e([s,o,v,o,o,s,o,o,o],y)). %6
%estado_final(e([s,o,s,o,o,s,o,o,o],x)).   
%  ?- estado_inicial(E), printTabuleiro(E).