female(pam).
female(liz).
female(pat).
female(ann).

male(tom).
male(bob).
male(jim).

parent(pam, bob).
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

mother(X, Y) :-
    parent(X, Y),
    female(X).

brother(X, Y) :-
    parent(Z, X),
    parent(Z,Y),
    male(X),
    dif(X, Y).

grandparent(X, Y) :-
    parent(X, Z),
    parent(Z, Y).

ancestor(X, Z) :-
    parent(X, Z).

ancestor(X, Z) :-
    parent(X, Y),
    ancestor(Y, Z).

