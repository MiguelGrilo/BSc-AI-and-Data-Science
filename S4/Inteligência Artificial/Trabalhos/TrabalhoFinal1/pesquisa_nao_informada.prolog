:- dynamic(maxNL/1).
:- dynamic(nos/1).
:- dynamic(nosE/1).

%Inicializa
maxNL(0).
nos(0).
nosE(1).
   
%Reinicializa
limpa:-retractall(maxNL(A)),
    retractall(nos(A)),
    retractall(nosE(A)),
    asserta(maxNL(0)),
    asserta(nos(0)),
    asserta(nosE(1)).
  
quantos:- nos(Ns),maxNL(NL),nosE(NE),
    nl, write(nos(visitados(Ns),lista(NL),expandidos(NE))),nl.
 
inc:- retract(nos(N)), N1 is N+1, asserta(nos(N1)). 
incE(M):- retract(nosE(N)), N1 is N+M, asserta(nosE(N1)).
  
actmax(N):- maxNL(N1), N1 >= N,!.
 actmax(N):- retract(maxNL(_N1)), asserta(maxNL(N)).
  
%Representacao_dos_nos
%no(Estado,no_pai,OperadorCusto,Profundidade)
 
pesquisa(Problema,Alg):-
    consult(Problema),
    limpa,
    estado_inicial(S0),
    pesquisa(Alg,[no(S0,[],[],0,0)],Solucao),
    nos(Ns),maxNL(NL),nosE(NE),
    nl, write(nos(visitados(Ns),lista(NL),expandidos(NE))),nl,
    limpa,
    escreve_seq_solucao(Solucao).

pesquisa(largura,Ln,Sol):- pesquisa_largura(Ln,Sol).
pesquisa(profundidade,Ln,Sol):- pesquisa_profundidade(Ln,Sol).  
pesquisa(profLim(P),Ln,Sol):- pesquisa_pLim(Ln,Sol,P).
pesquisa(profIt,Ln,Sol):- pesquisa_it(Ln,Sol,0).  
pesquisa(it,Ln,Sol):- pesquisa_it(Ln,Sol,1).
   
pesquisa_it(Ln,Sol,P):- pesquisa_pLim(Ln,Sol,P).
pesquisa_it(Ln,Sol,P):- P1 is P+1, pesquisa_it(Ln,Sol,P1).
  
%pesquisa_largura([],_):- !,fail. 
pesquisa_largura([no(E,Pai,Op,C,P)|_],no(E,Pai,Op,C,P)):- estado_final(E), inc.
pesquisa_largura([E|R],Sol):- inc,
    expande(E,Lseg), %esc(E),
    insere_fim(Lseg,R,Resto),
    length(Resto,N), actmax(N),
    pesquisa_largura(Resto,Sol).
 
expande(no(E,Pai,Op,C,P),L):-findall(no(En,no(E,Pai,Op,C,P),Opn,Cnn,P1),
    (op(E,Opn,En,Cn),P1 is P+1, Cnn is Cn+C,incE(1)), L).
 
pesquisa_profundidade([no(E,Pai,Op,C,P)|_],no(E,Pai,Op,C,P)):- estado_final(E), inc.
pesquisa_profundidade([E|R],Sol):- inc, %asserta(fechado(E)),
    expande(E,Lseg), %esc(E),
    insere_fim(R,Lseg,Resto),
    length(Resto,N), actmax(N),
    pesquisa_profundidade(Resto,Sol).
    
expandePl(no(_E,_Pai,_Op,_C,P),[],Pl):- Pl =< P, !.
expandePl(no(E,Pai,Op,C,P),L,_):- findall(no(En,no(E,Pai,Op,C,P),Opn,Cnn,P1),
    (op(E,Opn,En,Cn),P1 is P+1, Cnn is Cn+C, incE(1)),L).
 
insere_fim([],L,L):-!.
insere_fim(L,[],L):-!.
insere_fim(R,[A|S],[A|L]):- insere_fim(R,S,L).
 
%pesquisa_pLim([],_,Pl):- nos(A), write(plim(Pl,A)),nl, fail.  
pesquisa_pLim([no(E,Pai,Op,C,P)|_],no(E,Pai,Op,C,P),_):- 
    estado_final(E), inc.
pesquisa_pLim([E|R],Sol,Pl):- inc, expandePl(E,Lseg,Pl),  %esc(E),
    insere_fim(R,Lseg,Resto), 
    length(Resto,N), actmax(N),
    pesquisa_pLim(Resto,Sol,Pl).
 
escreve_seq_solucao(no(E,Pai,Op,Custo,Prof)):- write(custo(Custo)),nl,
    write(profundidade(Prof)),nl,
    escreve_seq_accoes(no(E,Pai,Op,_,_)).

escreve_seq_accoes([]).
escreve_seq_accoes(no(E,Pai,Op,_,_)):- escreve_seq_accoes(Pai),
    write(e(Op,E)),nl.
 
esc(A):- write(A), nl.