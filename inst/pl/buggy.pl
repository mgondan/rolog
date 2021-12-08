% Correct step from task to solution
expert(tratio(X, Mu, S, N), frac(X - Mu, S / sqrt(N))).

% Mistakes
buggy(frac(X - Mu, S / SQRTN), X - frac(Mu, S) / SQRTN).
buggy(sqrt(N), N).

% Apply expert and buggy rules
step(X, Y) :-
    expert(X, Y).

step(X, Y) :-
    buggy(X, Y).

% Enter expressions
step(X, Y) :-
    compound(X),
    mapargs(search, X, Y),
    dif(X, Y).

% Search through problem space
search(X, X).

search(X, Z) :-
    step(X, Y),
    search(Y, Z).
