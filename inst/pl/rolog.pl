:- use_module(library(terms)).
:- dynamic(r2pl_hook/2).
:- dynamic(pl2r_hook/2).

r2pl(X, Y),
    (atomic(X) ; compound(X)),
    r2pl_hook(X, Z)
 => r2pl(Z, Y).

r2pl(X, Y),
    compound(X)
 => mapargs(r2pl, X, Y).

r2pl(X, Y)
 => Y = X.

pl2r(X, Y),
    (atomic(X) ; compound(X)),
    pl2r_hook(X, Z)
 => pl2r(Z, Y).

pl2r(X, Y),
    compound(X)
 => mapargs(pl2r, X, Y).

pl2r(X, Y)
 => Y = X.

r2pl_hook(sin(X), cos(X)).
pl2r_hook(cos(X), tan(X)).

