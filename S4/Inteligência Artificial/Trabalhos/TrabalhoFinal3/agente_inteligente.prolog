joga :-
    [alfabeta],
    %[minmax],
    estado_inicial(Ei),
    printTabuleiro(Ei),
    jogar_loop(Ei, x).

jogar_loop(Estado, Jogador) :-
    (terminal(Estado) ->
        write('Fim do jogo!'), nl,
        valor(Estado, V, 0),
        format('Resultado: ~w~n', [V]),
        printTabuleiro(Estado);
        (Jogador = x -> % Humano joga
            format('Jogador ~w, escolha a posição (1-9):~n', [Jogador]),
            read(Pos),
            format('Jogador ~w, escolha a letra (s ou o):~n', [Jogador]),
            read(Letra),
            (op1(Estado, joga(Pos, Letra), NovoEstado) ->
                printTabuleiro(NovoEstado),
                inv(Jogador, ProximoJogador),
                jogar_loop(NovoEstado, ProximoJogador);
                write('Jogada inválida, tenta outra vez.'), nl,
                jogar_loop(Estado, Jogador)); 
            alfabeta(Estado, Op), %Agente joga (Jogador = y)
            %minimax_decidir(Estado, Op),
            format('Agente escolhe: ~w~n', [Op]),
            op1(Estado, Op, NovoEstado),
            printTabuleiro(NovoEstado),
            inv(Jogador, ProximoJogador),
            jogar_loop(NovoEstado, ProximoJogador)
        )
    ).

printTabuleiro(e([A,B,C,D,E,F,G,H,I], Jogador)) :-
    format('~w | ~w | ~w~n', [A,B,C]),
    format('~w | ~w | ~w~n', [D,E,F]),
    format('~w | ~w | ~w~n', [G,H,I]),
    format('Jogador atual: ~w~n~n', [Jogador]).