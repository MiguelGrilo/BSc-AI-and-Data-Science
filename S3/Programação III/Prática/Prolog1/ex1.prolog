use_module('/home/miguelgrilo/UNI-L58387-IACD/S3-P3/Práticas/pr1/1.pl').
%progenitor(PAI_MAE, FILHO_FILHA)

mulher(mariana).
mulher(beatriz).
mulher(alexandra).
mulher(maria_joana).
mulher(paula).
mulher(silvia).
mulher(anabela).

homem(jose).
homem(manuel).
homem(analidio).
homem(miguel).
homem(rafael).
homem(jose_rui).
homem(antonio).
homem(rui_filipe).
homem(tiago).
homem(rui).
homem(francisco).

progenitor(mariana, alexandra).
progenitor(jose, alexandra).
progenitor(beatriz, analidio).
progenitor(manuel, analidio).

progenitor(mariana, maria_joana).
progenitor(jose, maria_joana).
progenitor(beatriz, paula).
progenitor(manuel, paula).

progenitor(mariana, jose_rui).
progenitor(jose, jose_rui).
progenitor(beatriz, silvia).
progenitor(manuel, silvia).

progenitor(mariana, anabela).
progenitor(jose, anabela).

progenitor(maria_joana, rui_filipe).
progenitor(antonio, rui_filipe).

progenitor(anabela, tiago).
progenitor(rui, tiago).
progenitor(anabela, francisco).
progenitor(rui, francisco).

progenitor(alexandra, miguel).
progenitor(analidio, miguel).
progenitor(alexandra, rafael).
progenitor(analidio, rafael).

pai(A, B) :- 
     progenitor(A, B), homem(A).

mae(A, B) :- 
     progenitor(A, B), mulher(A).

irmao_generico(A, B) :- 
     pai(C, A), pai(C, B), mae(D, A), mae(D, B), A\=B.

irma(A, B) :- 
     pai(C, A), pai(C, B), mae(D, A), mae(D, B), mulher(A), A\=B.

avos(PAI, MAE, AVO_MATERNO, AVOO_MATERNA, AVO_PATERNO, AVOO_PATERNA, INDIVIDUO) :- 
     pai(PAI, INDIVIDUO), pai(AVO_PATERNO, PAI), mae(AVOO_PATERNA,PAI),
     mae(MAE, INDIVIDUO), mae(AVOO_MATERNA, MAE), pai(AVO_MATERNO, MAE).

tios(TIOS, INDIVIDUO) :- 
     progenitor(X, INDIVIDUO), irmao_generico(X, TIOS).

primo(PRIMO, INDIVIDUO) :- 
    progenitor(X, INDIVIDUO),
    progenitor(Y, PRIMO),
    irmao_generico(X, Y).

antepassado(ANTEPASSADO, INDIVIDUO) :-  %Antepassado direto
    progenitor(ANTEPASSADO, INDIVIDUO).
     % OU
antepassado(ANTEPASSADO, INDIVIDUO) :- 
    progenitor(X, INDIVIDUO),
    antepassado(ANTEPASSADO, X).

descendente(DESCENDENTE, INDIVIDUO) :-  %Descendente direto
    progenitor(INDIVIDUO, DESCENDENTE).
    % OU
descendente(DESCENDENTE, INDIVIDUO) :- 
    progenitor(INDIVIDUO, X),
    descendente(DESCENDENTE, X).

meio_irmao(A, B) :- 
    pai(P, A), pai(P, B), mae(M1, A), mae(M2, B), M1 \= M2, A \= B.
    % OU
meio_irmao(A, B) :- 
    mae(M, A), mae(M, B), pai(P1, A), pai(P2, B), P1 \= P2, A \= B.

primos_segundo_grau(A, B) :-
    progenitor(PAIS1, A), progenitor(PAIS2, B),
    progenitor(AVO1, PAIS1), progenitor(AVO2, PAIS2),
    irmao_generico(AVO1, AVO2).

parente(A, B) :- 
    antepassado(ANTEPASSADO, A),
    antepassado(ANTEPASSADO, B),
    A \= B.
    % OU
parente(A, B) :- 
    descendente(DESCENDENTE, A),
    descendente(DESCENDENTE, B),
    A \= B.

%ex13 TIRADO DO CHATGPT
%grau_parentesco(A, B, Grau) :-     % Grau de parentesco via ascendência comum
%    caminho_ascendencia(A, B, 0, Grau).
%
%grau_parentesco(A, B, Grau) :-     % Grau de parentesco via descendência comum
%    caminho_descendencia(A, B, 0, Grau).
%
%%:Caso base: Se A e B forem a mesma pessoa, o grau é 0
%caminho_ascendencia(A, A, Grau, Grau). 
%caminho_descendencia(A, A, Grau, Grau).
%
%caminho_ascendencia(A, B, GAtual, Grau) :-   % Caminho por ascendência (subindo a árvore)
%    progenitor(P, A),
%    GProx is GAtual + 1,
%    caminho_ascendencia(P, B, GProx, Grau).
%
%caminho_descendencia(A, B, GAtual, Grau) :-  % Caminho por descendência (descendo a árvore)
%    progenitor(A, F),
%    GProx is GAtual + 1,
%    caminho_descendencia(F, B, GProx, Grau).

id(XPTO, XPTO).     %??

%id(XPTO, XPTO).
%
%pai(A, B) :- progenitor(A, B), homem(A).
%mae(A, B) :- progenitor(A, B), mulher(A).
%
%avo(A, B) :- pai(A, X), pai(X, B).
%avo(A, B) :- mae(A, X), pai(X, B).
%avo(A, B) :- pai(A, X), mae(X, B).
%avo(A, B) :- mae(A, X), mae(X, B).
%
%irmao(A, B) :- homem(A), pai(X, A), pai(X, B), mae(Y, A), mae(Y, B), \+ id(A, B).
%irma(A, B) :- mulher(A), pai(X, A), pai(X, B), mae(Y, A), mae(Y, B), \+ id(A, B).
%
%tio(A, B) :- irmao(A, X), pai(X, B).
%tio(A, B) :- irmao(A, X), mae(X, B).
%tia(A, B) :- irma(A, X), pai(X, B).
%tia(A, B) :- irma(A, X), mae(X, B).
%
%primo(A, B) :- progenitor(X, A), progenitor(Y, B), irmao(X, Y).
%
%antepassado(A, B) :- progenitor(A, B).
%antepassado(A, B) :- progenitor(A, X), antepassado(X, B).
%
%descendente(A, B) :- progenitor(B, A).
%descendente(A, B) :- progenitor(B, X), antepassado(X, A).
%
%meio_irmao(A, B) :- homem(A), pai(X, A), pai(X, B), mae(Y, A), \+ mae(Y, B).
%meio_irmao(A, B) :- homem(A), mae(X, A), mae(X, B), pai(Y, A), \+ pai(Y, B).
%meia_irma(A, B) :- mulher(A), pai(X, A), pai(X, B), mae(Y, A), \+ mae(Y, B).
%meia_irma(A, B) :- mulher(A), mae(X, A), mae(X, B), pai(Y, A), \+ pai(Y, B).
%
%primo_2grau(A, B) :- avo(X, A), avo(Y, B), irmao(X, Y).
%primo_2grau(A, B) :- avo(X, A), avo(Y, B), irma(X, Y).
%primo_2grau(A, B) :- avo(X, A), avo(X, B), pai(PaiA, A), pai(PaiB, B), \+ irmao(PaiA, PaiB).
%primo_2grau(A, B) :- avo(X, A), avo(X, B), mae(MaeA, A), mae(MaeB, B), \+irma(MaeA, MaeB).
%primo_2grau(A, B) :- avo(X, A), avo(X, B), pai(PaiA, A), mae(MaeB, B), \+irmao(PaiA, MaeB).
%primo_2grau(A, B) :- avo(X, A), avo(X, B), mae(MaeA, A), pai(PaiB, B), \+irma(MaeA, PaiB).
%
%parente(A, B) :- antepassado(A, X), antepassado(B, X).