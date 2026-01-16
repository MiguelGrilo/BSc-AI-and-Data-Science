%1
homem('Afonso Henriques','rei de Portugal',1109).
homem('Henrique de Borgonha','conde de Portugal',1069).

homem('Sancho I','rei de Portugal',1154).
homem('Fernando II','rei de Leão',1137).
homem('Afonso IX', 'rei de Leão e Castela', 1171).
homem('Afonso II', 'rei de Portugal',1185).

homem('Sancho II', 'rei de Portugal',1207).
homem('Afonso III', 'rei de Portugal',1210).


mulher('Teresa de Castela', 'condessa de Portugal', 1080).
mulher('Mafalda', 'condessa de Saboia', 1125).
mulher('Urraca', 'infanta de Portugal',1151).
mulher('Dulce de Barcelona','infanta de Aragão',1160).
mulher('Berengária', 'infanta de Portugal',1194).
mulher('Urraca C','infanta de Castela',1186).


filho('Afonso Henriques','Henrique de Borgonha').
filho('Afonso Henriques','Teresa de Castela').
filho('Urraca','Afonso Henriques').
filho('Sancho I','Afonso Henriques').
filho('Urraca','Mafalda').
filho('Sancho I','Mafalda').
filho('Afonso IX','Urraca').
filho('Afonso IX','Fernando II').
filho('Afonso II','Sancho I').
filho('Afonso II','Dulce de Barcelona').
filho('Berengária','Sancho I').
filho('Berengária','Dulce de Barcelona').
filho('Sancho II','Afonso II').
filho('Afonso III','Afonso II').
filho('Sancho II','Urraca C').
filho('Afonso III','Urraca C').

%2
irmao(A, B) :-
    filho(A, C), filho(B, C), A\=B.
%3
primoDireito(A, B) :-
    irmao(C, D), filho(A, C), filho(B, D), C\=D.
%4
irmaos(A, Lista) :-
    findall(B, irmao(A, B), Lista).
%5
filhoPrimo(A, B) :-
    primoDireito(A, C),
    filho(C, B).
primoPais(A, B) :-
    filho(A, P1),
    filho(B, P2),
    irmao(P1, P2),
    P1\=P2.
primos(A, Lista) :-
    findall(B, primoDireito(A, B), L1),
    findall(B, filhoPrimo(A, B), L2),
    findall(B, primoPais(A, B), L3),
    append(L1, L2, Temp),
    append(Temp, L3, Lista).
%6
esposa(E, H) :-
    filho(A, H),
    filho(A, E).
%7
ascendente(X, []) :-
    \+filho(X, _).
ascendente(X, [P, M | R]) :- 
    filho(X, P), homem(P, _, _),
    filho(X, M), mulher(M, _, _),
    ascendente(P, L1), ascendente(M, L2),
    append(L1, L2, R).
%8
descendentes(X, []) :-
    \+filho(_, X).
descendentes(X, [F | R]) :-
    filho(F, X),
    descendentes(F, R).
%9
%10
%11
%12
%13
%14
%15
%16