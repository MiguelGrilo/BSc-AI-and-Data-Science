%working_directory(D) ~ unifica D com a diretoria corrente
%directory_files(D, Fs) ~ unifica Fs com a lista de ficheiros na diretoria D
%file_property(F, PROP) ~ unifica PROP com uma propriedade do ficheiro F; pode ser, nomeadamente, um dos seguintes: absolute_file_name(X), real_file_name(X), type(X), size(S)

%1
copy(IN, OUT) :- see(IN), tell(OUT), copy, seen, told.
copy :- get0(C), copy_(C).
copy_(-1) :- !.
%ALTERA O CARATÉR ATUAL CASO SEJA MAIÚSCULA
copy_(C) :- maiusc(C), M is C+32, put(M), copy.
%COPIA O CARATÉR SEM ALTERAÇÕES CASO JÁ SEJA MINÚSCULA
copy_(C) :- \+ maiusc(C), put(C), copy.

%minusc(L) :- 97 =< L, L =<122.
minusc(L) :- 0'a =< L, L =< 0'z.
maiusc(L) :- 0'A =< L, L =< 0'Z.

to_ln(IN, OUT) :- see(IN), tell(OUT), copy, seen, told.
to_ln :- copy.

%2
% Define o predicado cap(S) que é verdadeiro se S for uma capicua

capicua(S) :-
    % Converte a string em uma lista de caracteres
    % Verifica se a lista de caracteres é igual à sua reversão