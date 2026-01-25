% 1.
% a)

%   ?- pesquisa(jarros, largura).

% b)
%   i)
% Mudar estado final para estado_final((1, 2)) no ficheiro jarros.pl
%   ?- pesquisa(jarros, largura).

%   ii)
% Mudar estado final para estado_final((3, 2)) no ficheiro jarros.pl
%   ?- pesquisa(jarros, largura).

%   iii)
% Mudar estado final para estado_final((5, 1)) no ficheiro jarros.pl
%   ?- pesquisa(jarros, largura).

%   iv)
% Mudar estado final para estado_final((1, 1)) no ficheiro jarros.pl
%   ?- pesquisa(jarros, largura).

% c)
%   ?- trace.
%   ?- pesquisa(jarros, largura).
% (Em todos os exercícios anteriores)

% 2.
% 1
pesquisa_profundidade([no(E,Pai,Op,C,P)|_], no(E,Pai,Op,C,P)):- 
    estado_final(E).

pesquisa_profundidade([E|R], Sol):- 
    expande(E, Lseg), esc(E),
    insere_inicio(Lseg, R, Resto), % Diferença principal: insere no início (LIFO)
    pesquisa_profundidade(Resto, Sol).

insere_inicio([], L, L).
insere_inicio(L, [], L).
insere_inicio(R, S, L):- append(R, S, L). % Insere os novos nós no início

%   ?- pesquisa_profundidade([no((0,0),[],[],0,0)], Solucao).

% 2
pesquisa_profundidade_sem_ciclos([no(E,Pai,Op,C,P)|_], no(E,Pai,Op,C,P)):- 
    estado_final(E).

pesquisa_profundidade_sem_ciclos([E|R], Sol):- 
    expande(E, Lseg), esc(E),
    remove_ciclos(Lseg, [E|R], LsegFiltrado), % Remove estados repetidos
    insere_inicio(LsegFiltrado, R, Resto),
    pesquisa_profundidade_sem_ciclos(Resto, Sol).

remove_ciclos([], _, []).
remove_ciclos([no(E,_,_,_,_)|T], Visitados, Resto):- 
    member(no(E,_,_,_,_), Visitados), !, 
    remove_ciclos(T, Visitados, Resto).
remove_ciclos([H|T], Visitados, [H|Resto]):- 
    remove_ciclos(T, Visitados, Resto).

%   ?- pesquisa_profundidade_sem_ciclos([no((0,0),[],[],0,0)], Solucao).

% 3
pesquisa_profundidade_limitada([no(E,Pai,Op,C,P)|_], no(E,Pai,Op,C,P), _):- 
    estado_final(E).

pesquisa_profundidade_limitada([no(E,Pai,Op,C,P)|R], Sol, Limite):- 
    P < Limite, % Verifica se ainda está dentro do limite
    expande(no(E,Pai,Op,C,P), Lseg), esc(no(E,Pai,Op,C,P)),
    insere_inicio(Lseg, R, Resto),
    pesquisa_profundidade_limitada(Resto, Sol, Limite).

%   ?- pesquisa_profundidade_limitada([no((0,0),[],[],0,0)], Solucao, 5).

% 4
pesquisa_profundidade_iterativa(EstadoInicial, Solucao) :-
    pesquisa_profundidade_iterativa_lim([no(EstadoInicial,[],[],0,0)], Solucao, 1).

pesquisa_profundidade_iterativa_lim(Ln, Sol, Limite) :-
    pesquisa_profundidade_limitada(Ln, Sol, Limite).
pesquisa_profundidade_iterativa_lim(Ln, Sol, Limite) :-
    NovoLimite is Limite + 1,
    pesquisa_profundidade_iterativa_lim(Ln, Sol, NovoLimite).

%   ?- pesquisa_profundidade_iterativa((0,0), Solucao).

%5
pesquisa_largura_bidireccional(EstadoInicial, EstadoFinal, Solucao) :-
    pesquisa_bidireccional([[no(EstadoInicial,[],[],0,0)]], [[no(EstadoFinal,[],[],0,0)]], Solucao).

pesquisa_bidireccional(Front1, Front2, Solucao) :-
    interseccao(Front1, Front2, Solucao).
pesquisa_bidireccional(Front1, Front2, Solucao) :-
    expande_lista(Front1, NovoFront1),
    expande_lista(Front2, NovoFront2),
    pesquisa_bidireccional(NovoFront1, NovoFront2, Solucao).

expande_lista([], []).
expande_lista([E|R], NovoR) :-
    expande(E, Lseg), insere_fim(Lseg, R, NovoR).

interseccao([], _, _) :- fail.
interseccao([no(E,_,_,_,_)|_], Lista, E) :- member(no(E,_,_,_,_), Lista), !.
interseccao([_|T], Lista, E) :- interseccao(T, Lista, E).

%   ?- pesquisa_largura_bidireccional((0,0), (3,1), Solucao).
