%Estado_Inicial
%v(x(Linha, Coluna), Domínio, ValorAtribuído)
%Exercicio_1
estado_inicial(e([v(x(1,3),[1,2,3,4,5,6,7,8,9],_),
    v(x(1,4),[1,2,3,4,5,6,7,8,9],_),
    v(x(2,1),[1,2,3,4,5,6,7,8,9],_),
    v(x(2,2),[1,2,3,4,5,6,7,8,9],_),
    v(x(2,3),[1,2,3,4,5,6,7,8,9],_),
    v(x(2,4),[1,2,3,4,5,6,7,8,9],_),
    v(x(3,1),[1,2,3,4,5,6,7,8,9],_),
    v(x(3,2),[1,2,3,4,5,6,7,8,9],_),
    v(x(3,3),[1,2,3,4,5,6,7,8,9],_),
    v(x(4,1),[1,2,3,4,5,6,7,8,9],_),
    v(x(4,2),[1,2,3,4,5,6,7,8,9],_)], [])).
/*
%Exercicio_5
estado_inicial(e([v(x(1,1),[1,2,3,4,5,6,7,8,9],_),
    v(x(1,2),[1,2,3,4,5,6,7,8,9],_),
    v(x(1,3),[1,2,3,4,5,6,7,8,9],4),
    v(x(2,1),[1,2,3,4,5,6,7,8,9],_),
    v(x(2,2),[1,2,3,4,5,6,7,8,9],_),
    v(x(2,3),[1,2,3,4,5,6,7,8,9],_),
    v(x(2,4),[1,2,3,4,5,6,7,8,9],4),
    v(x(2,5),[1,2,3,4,5,6,7,8,9],_),
    v(x(3,2),[1,2,3,4,5,6,7,8,9],_),
    v(x(3,3),[1,2,3,4,5,6,7,8,9],_),
    v(x(3,4),[1,2,3,4,5,6,7,8,9],_),
    v(x(3,5),[1,2,3,4,5,6,7,8,9],_),
    v(x(4,1),[1,2,3,4,5,6,7,8,9],_),
    v(x(4,2),[1,2,3,4,5,6,7,8,9],2),
    v(x(4,3),[1,2,3,4,5,6,7,8,9],5),
    v(x(4,4),[1,2,3,4,5,6,7,8,9],_),
    v(x(5,1),[1,2,3,4,5,6,7,8,9],_),
    v(x(5,2),[1,2,3,4,5,6,7,8,9],8),
    v(x(5,3),[1,2,3,4,5,6,7,8,9],_)], [])).*/

%Diferencas_entre_pares_de_variaveis
%Exercicio_1    
diff(x(1,3),x(1,4)). diff(x(1,3),x(2,3)). diff(x(1,3),x(3,3)).
diff(x(1,4),x(2,4)). diff(x(2,1),x(2,2)). diff(x(2,1),x(2,3)). 
diff(x(2,1),x(2,4)). diff(x(2,1),x(3,1)). diff(x(2,1),x(4,1)).
diff(x(2,2),x(2,3)). diff(x(2,2),x(2,4)). diff(x(2,2),x(3,2)). 
diff(x(2,2),x(4,2)). diff(x(2,3),x(2,4)). diff(x(2,3),x(3,3)).
diff(x(3,1),x(3,2)). diff(x(3,1),x(3,3)). diff(x(3,1),x(4,1)).
diff(x(3,2),x(3,3)). diff(x(3,2),x(4,2)). diff(x(4,1),x(4,2)).
/*
%Exercicio_5
diff(x(1,1),x(1,2)). diff(x(1,1),x(1,3)). diff(x(1,1),x(2,1)).
diff(x(1,1),x(4,1)). diff(x(1,1),x(5,1)). diff(x(1,2),x(1,3)).
diff(x(1,2),x(2,2)). diff(x(1,2),x(3,2)). diff(x(1,2),x(4,2)).
diff(x(1,2),x(5,2)). diff(x(1,3),x(2,3)). diff(x(1,3),x(3,3)).
diff(x(1,3),x(4,3)). diff(x(1,3),x(5,3)). diff(x(2,1),x(2,2)).
diff(x(2,1),x(2,3)). diff(x(2,1),x(2,4)). diff(x(2,1),x(2,5)).
diff(x(2,1),x(4,1)). diff(x(2,1),x(5,1)). diff(x(2,2),x(2,3)). 
diff(x(2,2),x(2,4)). diff(x(2,2),x(2,5)). diff(x(2,2),x(3,2)). 
diff(x(2,2),x(4,2)). diff(x(2,2),x(5,2)). diff(x(2,3),x(2,4)).
diff(x(2,3),x(2,5)). diff(x(2,3),x(3,3)). diff(x(2,3),x(4,3)).
diff(x(2,3),x(5,3)). diff(x(2,4),x(2,5)). diff(x(2,4),x(3,4)).
diff(x(2,4),x(4,4)). diff(x(2,5),x(3,5)). diff(x(3,2),x(3,3)).
diff(x(3,2),x(3,4)). diff(x(3,2),x(3,5)). diff(x(3,2),x(4,2)).
diff(x(3,2),x(5,2)). diff(x(3,3),x(3,4)). diff(x(3,3),x(3,5)).
diff(x(3,3),x(4,3)). diff(x(3,3),x(5,3)). diff(x(3,3),x(3,4)). 
diff(x(3,4),x(3,5)). diff(x(3,4),x(4,4)). diff(x(4,1),x(4,2)). 
diff(x(4,1),x(4,3)). diff(x(4,1),x(4,4)). diff(x(4,1),x(5,1)). 
diff(x(4,2),x(4,3)). diff(x(4,2),x(4,4)). diff(x(4,2),x(5,2)). 
diff(x(4,3),x(4,4)). diff(x(4,3),x(5,3)). diff(x(5,1),x(5,2)). 
diff(x(5,1),x(5,3)).*/

todos_diferentes([]).
todos_diferentes([H|T]) :- \+ member(H, T), todos_diferentes(T).

sumlist([], 0).
sumlist([H|T], Soma) :- sumlist(T, Resto), Soma is H + Resto.

verifica_diferentes(Afect) :-
    \+ ( member(v(Xi, _, Vi), Afect), member(v(Xj, _, Vj), Afect),
    Xi \= Xj, (diff(Xi, Xj) ; diff(Xj, Xi)),
    nonvar(Vi), nonvar(Vj), Vi =:= Vj).

verifica_soma(Afect, Vars, Soma) :-
    findall(V, (member(X, Vars), member(v(X, _, V), Afect), nonvar(V)), Valores),
    ( length(Valores, L), length(Vars, L) ->
        sumlist(Valores, Soma), todos_diferentes(Valores);
        sumlist(Valores, SomaParcial), SomaParcial =< Soma, todos_diferentes(Valores)).

%Restricoes_Somas
%Exercicio_1
soma_restricoes([([x(1,3), x(1,4)], 13),
    ([x(1,3), x(2,3), x(3,3)], 24),
    ([x(1,4), x(2,4)], 15),
    ([x(2,1), x(2,2), x(2,3), x(2,4)], 24),
    ([x(3,1), x(3,2), x(3,3)], 23),
    ([x(4,1), x(4,2)], 11),
    ([x(2,1), x(3,1), x(4,1)], 23),
    ([x(2,2), x(3,2), x(4,2)], 9)]).
/*
%Exercicio_5
soma_restricoes([([x(1,1), x(1,2), x(1,3)], 20),
    ([x(2,1), x(2,2), x(2,3), x(2,4), x(2,5)], 23),
    ([x(3,2), x(3,3), x(3,4), x(3,5)], 14),
    ([x(4,1), x(4,2), x(4,3), x(4,4)], 23),
    ([x(5,1), x(5,2), x(5,3)], 19),
    ([x(1,1), x(2,1)], 13),
    ([x(4,1), x(5,1)], 11),
    ([x(1,2), x(2,2), x(3,2), x(4,2), x(5,2)], 26),
    ([x(1,3), x(2,3), x(3,3), x(4,3), x(5,3)], 28),
    ([x(2,4), x(3,4), x(4,4)], 18),
    ([x(2,5), x(3,5)], 3)]).*/

verifica_somas(Afect) :- soma_restricoes(Lista),
    forall(member((Vars, Soma), Lista), verifica_soma(Afect, Vars, Soma)).

ve_restricoes(e(_, Afect)) :- 
    verifica_diferentes(Afect),
    verifica_somas(Afect).


%Exercicio_2
algoritmo(Metodo) :-
    sol(Metodo, Sol, N),
    write('Solução: '), write(Sol), nl,
    write('Nós visitados: '), write(N), nl.

:- dynamic(nos/1).
nos(0).
inc :- retract(nos(N)), N1 is N+1, asserta(nos(N1)).

backtracking(Sol, N) :-
    retractall(nos(_)), asserta(nos(0)),
    estado_inicial(E0),
    back(E0, Sol),
    nos(N).

todas_solucoes_backtracking(Sol, N) :-
    retractall(nos(_)), asserta(nos(0)),
    findall(S, (estado_inicial(E0), back(E0, S)), Sol),
    length(Sol, Total),
    Total > 0,
    nos(N).

back(e([],A), A).
back(E, Sol) :-
    sucessor(E, E1),
    inc,
    ve_restricoes(E1),
    back(E1, Sol).

sucessor(e([v(N,D,V)|R],E),e(R,[v(N,D,V)|E])):- member(V,D).


%Exercicio_3
forward_checking(Sol, N) :-
    retractall(nos(_)), asserta(nos(0)),
    estado_inicial(E0),
    backtracking_forward(E0, Sol),
    nos(N).

todas_solucoes_forward(Sol, N) :-
    retractall(nos(_)), asserta(nos(0)),
    findall(S, (estado_inicial(E0), backtracking_forward(E0, S)), Sol),
    length(Sol, Total),
    Total > 0,
    nos(N).

forCheck(e(Lni,[v(N,D,V)|Li]), e(Lnii,[v(N,D,V)|Li])) :- corta(N, V, Lni, Lnii).

corta(_, _, [], []). %Lista_vazia
corta(N1, V1, [v(N2, D2, V2)|Resto], [v(N2, D2nova, V2)|RestoNovo]) :- %Variavel_relacionada_aplica_corte_dominio
    var(V2), ( diff(N1, N2) ; diff(N2, N1) ),
    delete(D2, V1, D2nova),
    D2nova \= [],
    corta(N1, V1, Resto, RestoNovo).
corta(N1, V1, [v(N2, D2, V2)|Resto], [v(N2, D2, V2)|RestoNovo]) :- %Variavel_nao_relacionada_nao_altera_dominio
    \+ diff(N1, N2), \+ diff(N2, N1),
    corta(N1, V1, Resto, RestoNovo).

backtracking_forward(e([],A),A).
backtracking_forward(E,Sol):- sucessor(E,E1), inc, ve_restricoes(E1),
    forCheck(E1,E2),
    backtracking_forward(E2,Sol).


%Exercicio_4
melhorado(Sol, N) :-
    retractall(nos(_)), asserta(nos(0)),
    estado_inicial(E0),
    backtracking_melhor(E0, Sol),
    nos(N).

todas_solucoes_melhorado(Sol, N) :-
    retractall(nos(_)), asserta(nos(0)),
    findall(S, (estado_inicial(E0), backtracking_melhor(E0, S)), Sol),
    length(Sol, Total),
    Total > 0,
    nos(N).

seleciona_melhor_var([H|T], Melhor, Restantes) :-
    seleciona_melhor_var_aux(T, H, Melhor, [], Restantes).

seleciona_melhor_var_aux([], Melhor, Melhor, Acc, Acc).
seleciona_melhor_var_aux([H|T], MelhorAtual, Melhor, Acc, Restantes) :-
    H = v(_, D1, _), MelhorAtual = v(_, D2, _),
    length(D1, L1), length(D2, L2),
    ( L1 < L2 ->
    seleciona_melhor_var_aux(T, H, Melhor, [MelhorAtual|Acc], Restantes);
    seleciona_melhor_var_aux(T, MelhorAtual, Melhor, [H|Acc], Restantes)).

sucessor_melhor(e(NaoInst, Atrib), e(NovaNaoInst, [v(N, D, V)|Atrib])) :-
    seleciona_melhor_var(NaoInst, v(N, D, _), Resto),
    D \= [], member(V, D),
    NovaNaoInst = Resto.

backtracking_melhor(e([], A), A).
backtracking_melhor(E, Sol) :-
    sucessor_melhor(E, E1),
    inc,
    ve_restricoes(E1),
    forCheck(E1, E2),
    backtracking_melhor(E2, Sol).

sol(backtracking, Sol, N) :- backtracking(Sol, N).
sol(todas_solucoes_backtracking, Sol, N) :- todas_solucoes_backtracking(Sol, N).
sol(forward_checking, Sol, N) :- forward_checking(Sol, N).
sol(todas_solucoes_forward, Sol, N) :- todas_solucoes_forward(Sol, N).
sol(melhorado, Sol, N) :- melhorado(Sol, N).
sol(todas_solucoes_melhorado, Sol, N) :- todas_solucoes_melhorado(Sol, N).