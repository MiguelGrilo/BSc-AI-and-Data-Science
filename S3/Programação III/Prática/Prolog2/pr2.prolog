num(z).
num(s(X)) :- num(X).

num(z, 0).
num(s(X), SY) :- num(X, Y), SY is Y+1.

le(z, _).
le(s(A), s(B)) :- le(A, B).

lt(z, s(_)).
lt(s(A), s(B)) :- lt(A, B).

soma(z, X, X) :- num(X).
soma(s(X), Y, s(Z)) :- soma(X, Y, Z).

sub(A, B, X) :- soma(X, B, A).

mult(z, X, z) :- num(X).
mult(s(A), B, X) :- mult(A, B, Y), soma(B, Y, X).

% div(z, A, X) :- mult(X, A, z).
div(A, B, X) :- mult(X, B, A).
div(A, B, Q, R) :- mult(B, Q, X), soma(X, R, A), lt(R, B).

dc(A, B, C) :- num(C), div(A, C, _), div(B, C, _).

mc(A, B, C) :- num(C), mult(A, _, C), mult(B, _, C).

min(A, B, A) :- le(A, B).
min(A, B, B) :- le(B, A).

max(A, B, A) :- le(B, A).
max(A, B, B) :- le(A, B).

primo(A) :- lt(B, A), B=s(s(_)), lt(C, A), mult(C, B, A), !, fail.
primo(A) :- num(A).