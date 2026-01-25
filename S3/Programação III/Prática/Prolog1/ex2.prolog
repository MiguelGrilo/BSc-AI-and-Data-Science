%use_module('/home/miguelgrilo/UNI-L58387-IACD/S3-P3/Práticas/pr1/2.pl').
%e(origem, chegada)

e(lisboa, santarem).
e(santarem, coimbra).
e(santarem, caldas).
e(caldas, lisboa).
e(coimbra, porto).
e(lisboa, evora).
e(evora, beja).
e(lisboa, faro).
e(beja, faro).

%1
a(X, Y).
a(X, Y) :- e(Y, X).

%2
cam(A, B) :-
	cam(A, B, A).

cam(A, B, C) :-
	a(A, B),
	nao_figura(B, C).
cam(A, B, C) :-
	a(A, D),
	nao_figura(D, C),
	cam(D, B, c(D, C)).

nao_figura(A, B) :-
	\+ figura(A, B).

figura(A, A).
figura(A, c(A, _)).
figura(A, c(_, B)) :-
	figura(A, B).

%3
comp(C, 1) :- atom(C).
comp(c(CID, CAM), L) :- 
    comp(CAM, LL),
    L is L+1.

%4
caminho_mais_curto(Origem, Destino, Caminho, Comprimento) :-
    findall((C, L), (cam(Origem, Destino, C), comp(C, L)), Caminhos),
    sort(Caminhos, [(ComprimentoMenor, CaminhoMenor) | _]),
    Caminho = CaminhoMenor.

%5
todas_cidades([lisboa, santarem, coimbra, caldas, porto, evora, beja, faro]).

caminho_todas_cidades(Caminho) :-
    todas_cidades(Cidades),
    permutation(Cidades, Caminho),
    caminho_valido(Caminho).

caminho_valido([_]).
caminho_valido([X, Y | Rest]) :-
    a(X, Y),
    caminho_valido([Y | Rest]).

%6
caminho_sem_repeticoes(Caminho) :-
    sort(Caminho, CaminhoSemRepeticoes),
    length(Caminho, L1),
    length(CaminhoSemRepeticoes, L1).

%7
caminho_mais_longo(Caminho, Comprimento) :-
    todas_cidades(Cidades),
    permutation(Cidades, Caminho),
    caminho_valido(Caminho),
    comp(Caminho, Comprimento),
    \+ (permutation(Cidades, OutroCaminho),
        caminho_valido(OutroCaminho),
        comp(OutroCaminho, OutroComprimento),
        OutroComprimento > Comprimento).
