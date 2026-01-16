%estado_inicial(e([Tabuleiro, SOSdex, SOSdey, Peças Jogadas],Jogador))
%estado_inicial(e([[v,v,v,v,v,v,v,v,v], SOSdex, SOSdey, pecas],jogador))
estado_inicial(e([[s,o,v,v,s,o,s,o,v], 0, 0, 6], x)).

inv(x,y).
inv(y,x).

% op1(+EstadoAtual, -Jogada, -EstadoSeguinte)
op1(e([Tab, Px, Py, N], J), (Pos, Letra), e([NovoTab, NovoPx, NovoPy, N1], J1)) :-
    nth0(Pos, Tab, v),                     
    member(Letra, [s,o]),                  
    replace(Tab, Pos, Letra, Tab1),        
    conta_sos(Tab1, Letra, Pos, J, Ganhos),
    atualiza_pontos(J, Ganhos, Px, Py, NovoPx, NovoPy),
    N1 is N + 1,                           
    inv(J, J1),
    NovoTab = Tab1.

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0, I1 is I - 1,
    replace(T, I1, X, R).

atualiza_pontos(x, Ganhos, Px, Py, Px1, Py) :- Px1 is Px + Ganhos.
atualiza_pontos(y, Ganhos, Px, Py, Px, Py1) :- Py1 is Py + Ganhos.

%direcoes([OffsetAntes, OffsetMeio, OffsetDepois])
direcoes([
    [-1, 0, +1],   % horizontal
    [-3, 0, +3],   % vertical
    [-4, 0, +4],   % diagonal principal
    [-2, 0, +2]    % diagonal secundária
]).

%conta_sos(+Tabuleiro, +Letra, +Pos, +Jogador, -TotalGanhos)
conta_sos(Tab, Letra, Pos, _, TotalGanhos) :-
    direcoes(Direcoes),
    findall(1, (
        member([A, 0, C], Direcoes),
        formar_sos(Tab, Letra, Pos, A, C)
    ), Ocorrencias),
    length(Ocorrencias, TotalGanhos).

%formar_sos(+Tabuleiro, +LetraJogada, +Pos, +OffsetAntes, +OffsetDepois)
formar_sos(Tab, o, Pos, A, C) :- % se colocou 'O', ela deve estar no centro: S O S
    P1 is Pos + A,
    P2 is Pos + C,
    dentro_tabuleiro(P1),
    dentro_tabuleiro(P2),
    nth0(P1, Tab, s),
    nth0(P2, Tab, s).

formar_sos(Tab, s, Pos, A, C) :- % se colocou 'S', ela pode estar na ponta: S O S
    P2 is Pos + A,
    P3 is Pos + C,
    dentro_tabuleiro(P2),
    dentro_tabuleiro(P3),
    nth0(P2, Tab, o),
    nth0(P3, Tab, s).

%dentro_tabuleiro(+Indice)
dentro_tabuleiro(I) :-
    I >= 0, I =< 8.

terminal(e([_, _, _, 9], _)).

valor(e([_, Px, Py, _], _),  1, _) :- Px > Py, !.
valor(e([_, Px, Py, _], _), -1, _) :- Px < Py, !.
valor(e([_, Px, Py, _], _),  0, _) :- Px =:= Py.