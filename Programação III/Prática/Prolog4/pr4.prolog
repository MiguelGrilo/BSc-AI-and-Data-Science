%1
sequencia(A, A, [A]).
sequencia(A, B, [A|X]) :-
    sequencia(s(A), B, X).

%2
double([], []).
double([InicioL1|RestoL1], [InicioL1, InicioL1|FimL2]) :-
    double(RestoL1, FimL2).

%3
adj(E1, E2, L) :- .

%num(z).
%num(s(X)) :- num(X).
%
%soma(z, X, X) :- num(X).
%soma(s(X), Y, s(Z)) :- soma(X, Y, Z).
%
%mult(z, X, z) :- num(X).
%mult(s(A), B, X) :- mult(A, B, Y), soma(B, Y, X).
%
%membro(X, [X|_]).
%membro(X, [_|L]) :- membro(X, L).
%membro(X, Y) :- catena(, [X|], Y).
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
%
%le(z, _).
%le(s(A), s(B)) :- le(A, B).
%
%sequencia(A, A, [A]).
%sequencia(A, B, L) :-
%    num(A),
%    num(B),
%    le(A, B),
%    sequencia(s(A), B, L1),
%    catena([A], L1, L).
%
%double([], []).
%double([X|T1], [X, X|T2]) :- double(T1, T2).
%
%adj(E1, E2, [E1, E2|_]).
%adj(E1, E2, [E2, E1|_]).
%adj(E1, E2, [_|L]) :- adj(E1, E2, L).
%
%sel(E, [E|L], L).
%sel(E, [X|L], [X|M]) :- sel(E, L, M).

soma(L, S) :- soma_(L, z, S).
soma_([], X, X).
soma_([A|B], X, S) :- soma(A, X, Y), soma_(B, Y, S).

produto_([], X, X).
produto_([A|B], X, P) :- mult(A, X, Y), produto_(B, Y, P).
produto(L, P) :- produto_(L, s(z), P).

ord([]).
ord([_]).
ord([X, Y|L]) :- le(X, Y), ord([Y|L]).

perm([], []).
perm([X|L1], L2) :- sel(X, L2, R), perm(L1, R).

sort(L, S) :- perm(L, S), ord(S). 