r_norm(N, L) :-
    r_eval('set.seed'(123)),
    r_eval(rnorm(N), L).
