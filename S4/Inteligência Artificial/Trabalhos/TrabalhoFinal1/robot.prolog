%Estado_Inicial

% [(Robot, Tipo, I, J)]
% e((I,J),[Itens]) %Uma lista que escreve o estado de cada casa
% Primeira casa: posição em linha (i) e coluna (j)
% Segunda casa: Lista para acumular os itens

%Estado_Inicial
estado_inicial(e(c(1,1),[])).

%Estado_Final
%Exemplo_1
estado_final(e(c(7,2),[b,a])).

%Exemplo_2
%estado_final(e(c(7,5),[b,a])).

%Elementos_Fixos (posições dos 'X' e das chaves)
%Exemplo_1
x(2,1).
x(6,1).
x(1,4).
x(2,4). 
x(3,4). 
x(4,4).
x(5,3). 
x(6,3). 
x(7,4).
x(6,7).

%Exemplo_2
%x(6,1).
%x(6,3).
%x(2,4).
%x(3,4).
%x(6,7).
%x(5,5).

%Objetos
%Exemplo_1
objeto(a, 4, 2).
objeto(b, 6, 2).

%Exemplo_2
%objeto(a, 7, 3).
%objeto(b, 4, 3).

valido(I,J) :-
    I >= 1,
    I =< 7,
    J >= 1,
    J =< 7,
    \+ x(I,J).

move((X,Y), (NX,Y)) :- NX is X+1. % direita
move((X,Y), (NX,Y)) :- NX is X-1. % esquerda
move((X,Y), (X,NY)) :- NY is Y+1. % cima
move((X,Y), (X,NY)) :- NY is Y-1. % baixo

op(e(c(I,J),Itens), anda, e(c(INovo,JNovo),Itens), 1) :-
    move((I,J), (INovo,JNovo)),
    valido(INovo, JNovo).

op(e(c(I,J),Itens), apanhar, e(c(I,J),[a|Itens]), 1) :-
    objeto(a,I,J),
    \+ member(a,Itens).

op(e(c(I,J),Itens), apanhar, e(c(I,J),[b|Itens]), 1) :-
    objeto(b,I,J),
    member(a,Itens),
    \+ member(b,Itens).

%Distancia_de_Manhattan_entre_duas_posicoes_c(I,J)
distancia(c(X1,Y1), c(X2,Y2), D) :-
    DX is abs(X1 - X2),
    DY is abs(Y1 - Y2),
    D is DX + DY.

%Heuristica_1
heuristica_h1(e(Pos, Itens), H) :-
    PosA = c(4,2),
    PosB = c(6,2),
    PosSaida = c(7,2),      %Trocar entre c(7,2) ou c(7,5) conforme o exemplo 1 ou 2 respectivamente
    (member(a, Itens) -> DA = 0 ; distancia(Pos, PosA, DA)),
    (member(b, Itens) -> DB = 0, DS = 0; distancia(PosA, PosB, DB), distancia(PosB, PosSaida, DS)),
    H is DA + DB + DS.

%Heuristica_2
heuristica_h2(e(PosAtual, Itens), H) :-
    PosSaida = c(7,2),
    distancia(PosAtual, PosSaida, D),
    length(Itens, N),
    H is D + 2 - N.

%Heuristica_1
h(E, H) :- heuristica_h1(E, H).
%Heuristica_2
%h(E, H) :- heuristica_h2(E, H).