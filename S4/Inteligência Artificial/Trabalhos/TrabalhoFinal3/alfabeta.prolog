:- dynamic(nos_visitados/1).
inicializa_contador :-
    retractall(nos_visitados(_)),
    asserta(nos_visitados(0)).

inc :-
    retract(nos_visitados(N)),
    N1 is N + 1,
    asserta(nos_visitados(N1)).

g(Jogo):- [Jogo], estado_inicial(Ei), alfabeta(Ei,Op), write(Op), nl.

alfabeta(Ei, terminou) :- terminal(Ei).
alfabeta(Ei, MelhorJogada) :- %x_MAX
    findall(V-Op, (op1(Ei, Op, Es), alfabeta_min(Es, V, 1, -10000, 10000)), L),
    escolhe_max(L, MelhorJogada).

alfabeta_min(Ei, Val, P, _, _) :- %terminal_MIN
    terminal(Ei), 
    valor(Ei, Val, P), !.
alfabeta_min(Ei, Val, P, Alfa, Beta) :- 
    inc,
    P1 is P + 1,
    V0 is 10000,
    findall(Es, op1(Ei, _, Es), Estados),
    processa_lista_min(Estados, P1, V0, Alfa, Beta, Val).

processa_lista_min([], _, V, _, _, V).
processa_lista_min([E|R], P, V, A, B, VFinal) :-
    alfabeta_max(E, V2, P, A, B),
    min(V, V2, Vmin),
    (Vmin =< A -> VFinal = Vmin ; 
     min(B, Vmin, B1),
     processa_lista_min(R, P, Vmin, A, B1, VFinal)).

alfabeta_max(Ei, Val, P, _, _) :- %terminal_MAX
    terminal(Ei), 
    valor(Ei, Val, P), !.
alfabeta_max(Ei, Val, P, Alfa, Beta) :- 
    inc,
    P1 is P + 1,
    V0 is -10000,
    findall(Es, op1(Ei, _, Es), Estados),
    processa_lista_max(Estados, P1, V0, Alfa, Beta, Val).

processa_lista_max([], _, V, _, _, V).
processa_lista_max([E|R], P, V, A, B, VFinal) :-
    alfabeta_min(E, V2, P, A, B),
    max(V, V2, Vmax),
    (Vmax >= B -> VFinal = Vmax ; 
    max(A, Vmax, A1),
    processa_lista_max(R, P, Vmax, A1, B, VFinal)).

min(A, B, A) :- A =< B, !.
min(_, B, B).
max(A, B, B) :- A =< B, !.
max(A, _, A).

escolhe_max([V-Op|Rest], MelhorOp) :- escolhe_max(Rest, V-Op, MelhorOp).
escolhe_max([], _-Op, Op).
escolhe_max([V1-_|T], V0-Op0, MelhorOp) :-
    V1 =< V0, !, escolhe_max(T, V0-Op0, MelhorOp).
escolhe_max([V1-Op1|T], _, MelhorOp) :-
    escolhe_max(T, V1-Op1, MelhorOp).