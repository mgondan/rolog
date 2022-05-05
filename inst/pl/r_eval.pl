r_seed(Seed) :-
    r_eval('set.seed'(Seed)).

r_norm(N, L) :-
    r_eval(rnorm(N), L).
