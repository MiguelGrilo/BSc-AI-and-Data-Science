%a
membro(X, [X|_]).
membro(X, [_|L]) :- membro(X, L).

%b
prefixo([], _).
prefixo([X|A], [X|B]) :- prefixo(A, B).

%c
sufixo(A, A).
sufixo(A, [_|B]) :- sufixo(A, B).

%d
sublista(S, L) :- prefixo(P, L), sufixo(S, P).

sublista(S, L) :- prefixo(S, L).
sublista(S, [_|L]) :- sublista(S, L).

%e
catena([], L, L).
catena([X|Xs], L, [X|Y]) :- catena(Xs, L, Y).

%f1
tamanho([], 0).
%tamanho([], z).
tamanho([_|T], X) :- compr(T, Y), X is Y+1.

%f2
tamanho(L, N) :- tamanho(L, 0, N).
tamanho([], Acc, Acc).
tamanho([_|T], Acc, N) :- 
    Acc1 is Acc + 1,
    tamanho(T, Acc1, N).

%g
inverte(L, R) :- rev(L, [], R).
inverte([], R, R).
inverte([A|B], X, R) :- rev(B, [A|X], R).

%g1
nrev([], []).
nrev([X|A], B) :-
    nrev(A, AR),
    catena(AR, [X], B).

%g2
nrev(L, R) :- nrev(L, [], R).
nrev([], Acc, Acc).
nrev([X|Xs], Acc, R) :- nrev(Xs, [X|Acc], R).



%membro(X, [X|_]).
%membro(X, [_|L]) :- membro(X, L).
%membro(X, Y) :- catena(, [X|], Y).
%
%% ser prefixo de: é o "começo" de, o início, a cabeça.
%
%prefixo([], _).
%prefixo([X|A], [X|B]) :- prefixo(A, B).
%prefixo(X, Y) :- catena(X, _, Y).
%
%sufixo(A, A).
%sufixo(A, [_|B]) :- sufixo(A, B).
%sufixo(X, Y) :- catena(_, X, Y).
%
%sublista(S, L) :- prefixo(P, L), sufixo(S, P).
%
%catena([], L, L).
%catena([X|Xs], L, [X|Y]) :- catena(Xs, L, Y).
%
%tamanho([], z).
%tamanho([_|T], s(X)) :- tamanho(T, X).