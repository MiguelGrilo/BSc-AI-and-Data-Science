accao(agarra(P, E),[maolivre(x), ocupa(P,E), esta_em(E)], [na_mao(P), livre(E)], [maolivre(x), ocupa(P,E)]):- 
    member(P,[1,2,3,4,5,6,7,8]), member(E,[a,b,c,d,e,f,g,h,i]).

accao(larga(P, E),[livre(E), na_mao(P), esta_em(E)], [maolivre(x), ocupa(P,E)], [livre(E), na_mao(P)]):-
    member(P,[1,2,3,4,5,6,7,8]), member(E,[a,b,c,d,e,f,g,h,i]).

accao(move_cima(E1,E2),[esta_em(E1), sobre(E2,E1)], [esta_em(E2)], [esta_em(E1)]):- 
    member(E1,[a,b,c,d,e,f,g,h,i]), member(E2,[a,b,c,d,e,f,g,h,i]), E1\=E2.

accao(move_baixo(E1,E2),[esta_em(E1), sobre(E1,E2)], [esta_em(E2)], [esta_em(E1)]):- 
    member(E1,[a,b,c,d,e,f,g,h,i]), member(E2,[a,b,c,d,e,f,g,h,i]), E1\=E2.

accao(move_esquerda(E1,E2),[esta_em(E1), esquerda(E2,E1)], [esta_em(E2)], [esta_em(E1)]):- 
    member(E1,[a,b,c,d,e,f,g,h,i]), member(E2,[a,b,c,d,e,f,g,h,i]), E1\=E2.

accao(move_direita(E1,E2),[esta_em(E1), esquerda(E1,E2)], [esta_em(E2)], [esta_em(E1)]):- 
    member(E1,[a,b,c,d,e,f,g,h,i]), member(E2,[a,b,c,d,e,f,g,h,i]), E1\=E2.

% estado_final([sobre(a,d), sobre(d,g), sobre(b,e), sobre(e,h), sobre(c,f), sobre(f,i), 
%                esquerda(a,b), esquerda(b,c), esquerda(d,e), esquerda(e,f), esquerda(g,h), esquerda(h,i),
%                livre(i), maolivre(x), esta_em(i), ocupa(1,a), ocupa(2,b), ocupa(3,c), ocupa(4,d),
%                ocupa(5,e), ocupa(6,g), ocupa(7,h), ocupa(8,f)]).

estado_inicial([sobre(a,d), sobre(d,g), sobre(b,e), sobre(e,h), sobre(c,f), sobre(f,i), 
    esquerda(a,b), esquerda(b,c), esquerda(d,e), esquerda(e,f), esquerda(g,h), esquerda(h,i),
    livre(e), maolivre(x), esta_em(a), ocupa(1,a), ocupa(2,b), ocupa(3,c), ocupa(4,d),
    ocupa(5,f), ocupa(6,g), ocupa(7,h), ocupa(8,i)]).

%estado_final([sobre(a,d), sobre(d,g), sobre(b,e), sobre(e,h), sobre(c,f), sobre(f,i), 
%                esquerda(a,b), esquerda(b,c), esquerda(d,e), esquerda(e,f), esquerda(g,h), esquerda(h,i),
%                livre(e), maolivre(x), esta_em(b), ocupa(1,a), ocupa(2,b), ocupa(3,c), ocupa(4,d),
%                ocupa(5,f), ocupa(6,g), ocupa(7,h), ocupa(8,i)]).

estado_final([sobre(a,d), sobre(d,g), sobre(b,e), sobre(e,h), sobre(c,f), sobre(f,i), 
%                esquerda(a,b), esquerda(b,c), esquerda(d,e), esquerda(e,f), esquerda(g,h), esquerda(h,i),
    livre(f), maolivre(x), esta_em(f), ocupa(1,a), ocupa(2,b), ocupa(3,c), ocupa(4,d),
    ocupa(5,e), ocupa(6,g), ocupa(7,h), ocupa(8,i)]).
