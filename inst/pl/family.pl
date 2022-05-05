parent(pam, bob).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

ancestor(X, Z) :-
    parent(X, Z).

ancestor(X, Z) :-
    parent(X, Y),
    ancestor(Y, Z).

