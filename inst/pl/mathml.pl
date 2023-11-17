:- discontiguous test/0, math/4, current/3, paren/3, prec/3, type/3, denoting/3, ml/3.
:- use_module(library(http/html_write)).

%
% R interface
%
r2mathml(A, X) :-
    r2mathml([], A, X).

r2mathml(Flags, A, S) :-
    mathml(Flags, A, M),
    html(M, X, []),
    maplist(atom_string, X, S).

mathml(Flags, A, X) :-
    ml(Flags, A, M),
    denoting(Flags, A, Denoting),
    ml(Flags, with(Denoting), With),
    !, X = [math(M), With].

mathml(Flags, A, X) :-
    ml(Flags, "Conversion failed", X),
    writeln(A).

%
% Macros
%
ml(Flags, A, X),
    math(Flags, A, New, M),
    dif(Flags-A, New-M)
 => ml(New, M, X).

math(Flags, A, New, X),
    member(replace(A, _), Flags)
 => select(replace(A, X), Flags, New).

%
% Greek letters
%
math(Flags, A, New, X),
    atom(A),
    memberchk(A, [alpha, beta, gamma, delta, epsilon, varepsilon, zeta,
        eta, theta, vartheta, iota, kappa, lambda, mu, nu, xi, pi, rho,
        sigma, tau, upsilon, phi, varphi, chi, psi, omega, 'Gamma',
        'Delta', 'Theta', 'Lambda', 'Xi', 'Pi', 'Sigma', 'Upsilon',
        'Phi', 'Psi', 'Omega'])
 => New = Flags,
    X = greek(A).

ml(_Flags, greek(A), X) =>
    X = mi(&(A)).

type(_Flags, greek(_), Type)
 => Type = atomic.

denoting(_Flags, greek(_), Den)
 => Den = [].

test :- test(epsilon).

%
% Sum over index
%
ml(Flags, sum(I, From, To, A), M)
 => ml(Flags, I = From, XFrom),
    ml(Flags, To, XTo),
    ml(Flags, A, X),
    M = mrow([munderover([mo(&(sum)), XFrom, XTo]), X]).

paren(Flags, sum(_, _, _, A), Paren)
 => paren(Flags, A, Paren).

prec(_Flags, sum(_, _, _, _), Prec)
 => current(Prec, yfx, +).

test :- test(sum(i, 1, 10, i)).

%
% Integrate over range
%
% extract value
math(Flags, $(integrate(Fn, Lower, Upper), value), New, M)
 => Flags = New,
    M = integrate(Fn, Lower, Upper).

% with named arguments
math(Flags, integrate(f=Fn, lower=Lower, upper=Upper), New, M)
 => Flags = New,
    M = integrate(Fn, Lower, Upper).

% No argument names
math(Flags, integrate(Fn, Lower, Upper), New, M)
 => Flags = New,
    r_eval('['(formalArgs(args(Fn)), 1), Arg1),
    atom_string(DX, Arg1),
    M = integrate(fn(Fn, [DX]), Lower, Upper, DX).

% Internal
ml(Flags, integrate(Fn, From, To, DX), M)
 => ml(Flags, Fn, XFn),
    ml(Flags, From, XFrom),
    ml(Flags, To, XTo),
    ml(Flags, DX, XDX),
    ml(Flags, space, Space),
    M = mrow([munderover([mo(&(int)), XFrom, XTo]), XFn, Space, mi(d), XDX]).

paren(Flags, integrate(_, _, _, A), Paren)
 => paren(Flags, A, Paren).

prec(_Flags, integrate(_, _, _, _), Prec)
 => current(Prec, yfx, *).

test :- test(integrate(sin, 0, pi)).

% hats
ml(Flags, hat(A), M)
 => ml(Flags, A, X),
	M = mover(accent(true), [X, mo(&('Hat'))]).

paren(Flags, hat(A), Paren)
 => paren(Flags, A, Paren).

prec(Flags, hat(A), Prec)
 => prec(Flags, A, Prec).

type(Flags, hat(A), Type)
 => type(Flags, A, Type).

test :- test(hat('K')).

test :- test(hat('K'^2)).
test :- test(hat('K')^2).
test :- test(hat('sigma')^2).

% tilde
ml(Flags, tilde(A), M)
 => ml(Flags, A, X),
	M = mover(accent(true), [X, mo(&(tilde))]).

paren(Flags, tilde(A), Paren)
 => paren(Flags, A, Paren).

prec(Flags, tilde(A), Prec)
 => prec(Flags, A, Prec).

type(Flags, tilde(A), Type)
 => type(Flags, A, Type).

test :- test(tilde('D')).

test :- test(tilde('X')^2).

%
% Booleans
%
math(Flags, A, New, X),
    atom(A),
    memberchk(A, ['TRUE', 'FALSE'])
 => New = Flags,
    X = boolean(A).

denoting(_Flags, boolean(_), Den)
 => Den = [].

%
% Space
%
math(Flags, space, New, X)
 => New = Flags,
    X = space(thinmathspace).

ml(_Flags, space(Width), M)
 => M = mspace(width(Width), []).

denoting(_Flags, space(_), Den)
 => Den = [].

test :- test(space).

%
% Symbols/Identifiers
%
math(Flags, A, New, X),
    atom(A)
 => New = Flags,
    X = ident(A).

ml(_Flags, ident(A), X)
 => X = mi(A).

type(_Flags, ident(_), Type)
 => Type = atomic.

denoting(_Flags, ident(_), Den)
 => Den = [].

test :- test(i).

%
% Upright text
%
math(Flags, A, New, X),
    string(A)
 => New = Flags,
    X = text(A).

ml(_Flags, text(A), X)
 => X = mtext(A).

type(_Flags, text(_), Type)
 => Type = atomic.

denoting(_Flags, text(_), Den)
 => Den = [].

test :- test("Text").

%
% Mathematical signs
%
ml(_Flags, sign(A), X)
 => X = mo(A).

prec(_Flags, sign(A), Prec),
    current(P, _Fix, A)
 => Prec = P.

current(Prec, yfx, &(sdot))
 => current_op(Prec, yfx, *).

denoting(_Flags, sign(_), Den)
 => Den = [].

test :- test(sign(&(sdot))).

%
% Indices like s_D
%
math(Flags, '['(A, Idx), New, X)
 => math(Flags, sub(A, Idx), New, X).

%
% Check for sub(sup(A, Power), Index)
%
math(Flags, sub(A, Idx), New, X),
    type(Flags, A, sup(Bas, Pwr))
 => New = [replace(sup(Bas, Pwr), subsup(Bas, Idx, Pwr)) | Flags],
    X = A.

%
%
% Render
%
math(Flags, sub(A, Idx), New, X),
    prec(Flags, sub(A, Idx), Outer),
    prec(Flags, A, Inner),
    Outer < Inner
 => New = Flags,
    X = sub(paren(A), Idx).

ml(Flags, sub(A, B), M)
 => ml(Flags, A, X),
    ml(Flags, B, Y),
    M = msub([X, Y]).

paren(Flags, sub(A, _), Paren)
 => paren(Flags, A, Paren).

prec(Flags, sub(A, _), Prec)
 => prec(Flags, A, Prec).

type(_Flags, sub(A, B), Type)
 => Type = sub(A, B).

test :- test(sub(s, 'D')).
test :- test(sub(s^r, 'D')).

%
% Powers like s^D
%
% Check for sup(sub(A, Index), Power)
%
math(Flags, sup(A, Pwr), New, X),
    type(Flags, A, sub(Bas, Idx))
 => New = [replace(sub(Bas, Idx), subsup(Bas, Idx, Pwr)) | Flags],
    X = A.

%
% Render
%
math(Flags, sup(A, Pwr), New, X),
    prec(Flags, sup(A, Pwr), Outer),
    prec(Flags, A, Inner),
    Outer < Inner
 => New = Flags,
    X = sup(paren(A), Pwr).

ml(Flags, sup(A, B), M)
 => ml(Flags, A, X),
    ml(Flags, B, Y),
    M = msup([X, Y]).

paren(Flags, sup(A, _), Paren)
 => paren(Flags, A, Paren).

prec(_Flags, sup(_, _), Prec)
 => current(Prec, xfy, ^).

type(_Flags, sup(A, B), Type)
 => Type = sup(A, B).

test :- test(sub(s, 'D')).
test :- test(sub(s^2, 'D')).

%
% Index and Exponent: s_D^2
%
math(Flags, subsup(A, Idx, Pwr), New, X),
    prec(Flags, subsup(A, Idx, Pwr), Outer),
    prec(Flags, A, Inner),
    Outer < Inner
 => New = Flags,
    X = subsup(paren(A), Idx, Pwr).

ml(Flags, subsup(A, B, C), M)
 => ml(Flags, A, X),
    ml(Flags, B, Y),
    ml(Flags, C, Z),
    M = msubsup([X, Y, Z]).

paren(Flags, subsup(A, _, _), Paren)
 => paren(Flags, A, Paren).

prec(Flags, subsup(A, _, C), Prec)
 => prec(Flags, sup(A, C), Prec).

type(_Flags, subsup(A, B, C), Type)
 => Type = subsup(A, B, C).

test :- test(subsup(s, 'D', r)).

%
% Numbers
%
math(Flags, A, New, X),
    integer(A),
    A >= 0
 => New = Flags,
    X = posint(A).

math(Flags, A, New, X),
    integer(A)
 => New = Flags,
    X = integer(A).

math(Flags, integer(A), New, X),
    A >= 0
 => New = Flags,
    X = posint(A).

math(Flags, integer(A), New, X),
    A < 0
 => New = Flags,
    Abs is abs(A),
    X = -posint(Abs).

math(Flags, A, New, X),
    number(A),
    A >= 0
 => New = Flags,
    X = pos(A).

math(Flags, A, New, X),
    number(A)
 => New = Flags,
    X = number(A).

denoting(_Flags, posint(_), Den)
 => Den = [].

denoting(_Flags, pos(_), Den)
 => Den = [].

denoting(_Flags, number(_), Den)
 => Den = [].

ml(_Flags, posint(A), M)
 => M = mn(A).

ml(_Flags, pos(1.0Inf), M)
 => M = mi(&('#x221E')).

ml(Flags, pos(A), M)
 => option(round(D), Flags, 2),
    D =< 99,
    format(codes(X), '~99f', [A]),
    nth1(Dot, X, 46),
    N is Dot + D,
    findall(E, (nth1(I, X, E), I =< N), Round),
    string_codes(S, Round),
    M = mn(S).

test :- test(3).
test :- test(-3).
test :- test(pi).

math(Flags, number(A), New, X),
    A < 0
 => New = Flags,
    Abs is abs(A),
    X = -pos(Abs).

math(Flags, number(A), New, X)
 => New = Flags,
    X = pos(A).

test :- test(-pi).

%
% Operators
%
math(Flags, A = B, New, X)
 => New = Flags,
    current_op(Prec, xfx, =),
    X = yfy(Prec, =, A, B).

math(Flags, A \= B, New, X)
 => New = Flags,
    current_op(Prec, xfx, \=),
    X = xfx(Prec, &(ne), A, B).

math(Flags, A < B, New, X)
 => New = Flags,
    current_op(Prec, xfx, <),
    X = yfy(Prec, <, A, B).

math(Flags, A =< B, New, X)
 => New = Flags,
    current_op(Prec, xfx, =<),
    X = yfy(Prec, &(le), A, B).

math(Flags, ~(A, B), New, X)
 => New = Flags,
    current_op(Prec, xfx, =),
    X = yfy(Prec, &('Tilde'), A, B).

math(Flags, A > B, New, X)
 => New = Flags,
    current_op(Prec, xfx, >),
    X = yfy(Prec, >, A, B).

math(Flags, A >= B, New, X)
 => New = Flags,
    current_op(Prec, xfx, >=),
    X = yfy(Prec, &(ge), A, B).

math(Flags, +A, New, X)
 => New = Flags,
    current_op(Prec, yfx, +),
    X = fy(Prec, +, A).

math(Flags, A + B, New, X)
 => New = Flags,
    current_op(Prec, yfx, +),
    X = yfy(Prec, +, A, B).

math(Flags, -A, New, X)
 => New = Flags,
    current_op(Prec, yfx, -),
    X = fy(Prec, -, A).

math(Flags, A - B, New, X)
 => New = Flags,
    current_op(Prec, yfx, -),
    X = yfy(Prec, -, A, B).

% Use dot or no dot instead of asterisk
math(Flags, A * B, New, X),
    type(Flags, B, Type),
    Type = atomic
 => New = Flags,
    X = nodot(A, B).

math(Flags, A * B, New, X)
 => New = Flags,
    X = dot(A, B).

test :- test(a * b).
test :- test(a * (b * c)).
test :- test((a * b) * c).
test :- test((2 * b) * c).
test :- test((-2 * b) * c).
test :- test((-2 * 2) * c).
test :- test((-2 * -2) * c).

math(Flags, dot(A, B), New, X)
 => New = Flags,
    current_op(Prec, yfx, *),
    X = yfy(Prec, &(sdot), A, B).

math(Flags, nodot(A, B), New, X)
 => New = Flags,
    current_op(Prec, yfx, *),
    X = yfy(Prec, &('#x2062'), A, B).

math(Flags, A / B, New, X)
 => New = Flags,
    current_op(Prec, yfx, /),
    X = yfx(Prec, /, A, B).

math(Flags, (A ; B), New, X)
 => New = Flags,
    current_op(Prec, xfy, ;),
    X = xfy(Prec, ;, A, B).

math(Flags, A^B, New, X)
 => New = Flags,
    X = sup(A, B).

%
% Render
%
ml(Flags, fy(Prec, Op, A), M)
 => ml(Flags, sign(Op), S),
    ml(Flags, right(Prec, A), X),
    M = mrow([S, X]).

ml(Flags, xfx(Prec, Op, A, B), M)
 => ml(Flags, left(Prec-1, A), X),
    ml(Flags, sign(Op), S),
    ml(Flags, right(Prec-1, B), Y),
    M = mrow([X, S, Y]).

ml(Flags, yfx(Prec, Op, A, B), M)
 => ml(Flags, left(Prec, A), X),
    ml(Flags, sign(Op), S),
    ml(Flags, right(Prec-1, B), Y),
    M = mrow([X, S, Y]).

ml(Flags, xfy(Prec, Op, A, B), M)
 => ml(Flags, left(Prec-1, A), X),
    ml(Flags, sign(Op), S),
    ml(Flags, right(Prec, B), Y),
    M = mrow([X, S, Y]).

ml(Flags, yfy(Prec, Op, A, B), M)
 => ml(Flags, left(Prec, A), X),
    ml(Flags, sign(Op), S),
    ml(Flags, right(Prec, B), Y),
    M = mrow([X, S, Y]).

denoting(Flags, fy(_, A, B), Den)
 => denoting(Flags, A, DenA),
    denoting(Flags, B, DenB),
    append(DenA, DenB, Den).

denoting(Flags, xfx(_, _, A, B), Den)
 => denoting(Flags, A, DenA),
    denoting(Flags, B, DenB),
    append(DenA, DenB, Den).

denoting(Flags, xfy(_, _, A, B), Den)
 => denoting(Flags, A, DenA),
    denoting(Flags, B, DenB),
    append(DenA, DenB, Den).

denoting(Flags, yfx(_, _, A, B), Den)
 => denoting(Flags, A, DenA),
    denoting(Flags, B, DenB),
    append(DenA, DenB, Den).

denoting(Flags, yfy(_, _, A, B), Den)
 => denoting(Flags, A, DenA),
    denoting(Flags, B, DenB),
    append(DenA, DenB, Den).

prec(_Flags, fy(P, _, _), Prec)
 => Prec = P.

prec(_Flags, xfx(P, _, _, _), Prec)
 => Prec = P.

prec(_Flags, yfx(P, _, _, _), Prec)
 => Prec = P.

prec(_Flags, xfy(P, _, _, _), Prec)
 => Prec = P.

prec(_Flags, yfy(P, _, _, _), Prec)
 => Prec = P.

math(Flags, left(Prec, A), New, X),
    prec(Flags, A, P),
    P > Prec
 => New = Flags,
    X = paren(A).

math(Flags, left(_, A), New, X)
 => New = Flags,
    X = A.

math(Flags, right(Prec, A), New, X)
 => New = Flags,
    X = left(Prec, A).

denoting(Flags, left(_, A), Den)
 => denoting(Flags, A, Den).

denoting(Flags, right(_, A), Den)
 => denoting(Flags, A, Den).

test :- test(a * b).
test :- test((a + b) * (c + d)).
test :- test(a * b + c * d).
test :- test(a + b + c + d).
test :- test((a - b) - (c + d)).
test :- test(dot((a - b), (c + d))).
test :- test(-2 * -2).

%
% Abbreviations
%
% with s^2_pool denoting the pooled variance
%
ml(Flags, with(A, _, _), X)
 => ml(Flags, A, X).

paren(Flags, with(A, _, _), Paren)
 => paren(Flags, A, Paren).

prec(Flags, with(A, _, _), Prec)
 => prec(Flags, A, Prec).

type(Flags, with(A, _, _), Type)
 => type(Flags, A, Type).

denoting(Flags, with(A, Expr, Info), Den)
 => denoting(Flags, Expr, T),
    Den = [denoting(A, Expr, Info) | T].

test :-
    S2P = with(sub(s, "pool")^2,
                   frac((sub('N', "A") - 1) * sub(s, "A")^2 +
                        (sub('N', "B") - 1) * sub(s, "B")^2,
                        sub('N', "A") + sub('N', "B") - 2),
                   "the pooled variance"),
    test(frac(sub(overline('X'), "A") - sub(overline('X'), "B"),
                  sqrt(nodot(S2P, 1/sub('N', "A") + 1/sub('N', "B"))))).

%
% Expand abbreviations
%
ml(Flags, denoting(A, Expr, Info), X)
 => ml(Flags, list(space, [A = Expr, "denoting", Info]), X).

type(Flags, denoting(A, _, _), Type)
 => type(Flags, A, Type).

denoting(_Flags, denoting(_, _, _), Den)
 => Den = [].

%
% Collect abbreviations
%
ml(Flags, with(Abbreviations), X)
 => sort(Abbreviations, Sorted), % remove duplicates
    ml(Flags, with_(Sorted), X).

ml(_Flags, with_([]), W)
 => W = " ".

ml(Flags, with_([A]), W)
 => ml(Flags, A, X),
    W = span([", with", &(nbsp), math(X)]).

ml(Flags, with_([A, B | T]), W)
 => ml(Flags, A, X),
    ml(Flags, and([B | T]), Y),
    W = span([", with", &(nbsp), math(X) | Y]).

ml(_Flags, and([]), W)
 => W = ".".

ml(Flags, and([A | T]), W)
 => ml(Flags, A, X),
    ml(Flags, and(T), Y),
    W = span([", and", &(nbsp), math(X) | Y]).

%
% Parentheses
%
ml(Flags, paren(A), X),
    paren(Flags, A, P),
    0 is P mod 3
 => ml(Flags, parentheses(A), X).

ml(Flags, paren(A), X),
    paren(Flags, A, P),
    1 is P mod 3
 => ml(Flags, brackets(A), X).

ml(Flags, paren(A), X),
    paren(Flags, A, P),
    2 is P mod 3
 => ml(Flags, braces(A), X).

paren(Flags, paren(A), Paren)
 => paren(Flags, A, P),
    Paren is P + 1.

ml(Flags, parentheses(A), M)
 => ml(Flags, A, X),
    M = mrow([mo('('), X, mo(')')]).

paren(_Flags, parentheses(_), Paren)
 => Paren is 1.

ml(Flags, brackets(A), M)
 => ml(Flags, A, X),
    M = mrow([mo('['), X, mo(']')]).

paren(_Flags, brackets(_), Paren)
 => Paren is 2.

ml(Flags, braces(A), M)
 => ml(Flags, A, X),
    M = mrow([mo('{'), X, mo('}')]).

paren(_Flags, braces(_), Paren)
 => Paren is 3.

test :- test(paren(paren(paren(paren(i))))).

%
% Lists of things
%
math(Flags, [H | T], New, M)
 => Flags = New, 
    M = list(space, [H | T]).

ml(Flags, list(_, [A]), M)
 => ml(Flags, A, M).

ml(Flags, list(Sep, [A, B | T]), M)
 => ml(Flags, A, X),
    ml(Flags, tail(Sep, [B | T]), Y),
    M = mrow([X | Y]).

ml(Flags, tail(Sep, [A]), M)
 => ml(Flags, Sep, S),
    ml(Flags, A, X),
    M = [S, X].

ml(Flags, tail(Sep, [A, B | T]), M)
 => ml(Flags, Sep, S),
    ml(Flags, A, X),
    ml(Flags, tail(Sep, [B | T]), Y),
    M = [S, X | Y].

paren(Flags, list(_, List), Paren)
 => maplist(paren(Flags), List, Parens),
    max_list(Parens, Paren).

prec(Flags, list(_, [A]), Prec)
 => prec(Flags, A, Prec).

prec(Flags, list(Sep, [_, _ | _]), Prec)
 => prec(Flags, Sep, Prec).

denoting(Flags, list(_, L), Den)
 => maplist(denoting(Flags), L, List),
    append(List, Den).

test :- test(list(space, [i, j, k])).

%
% Fractions
%
ml(Flags, frac(N, D), M)
 => ml(Flags, N, X),
    ml(Flags, D, Y),
    M = mfrac([X, Y]).

paren(_Flags, frac(_, _), Paren)
 => Paren = 0.

prec(_Flags, frac(_, _), Prec)
 => current(P, yfx, /),
    Prec is P - 1.

test :- test(frac(1, pi)).

%
% Large fraction
%
math(Flags, dfrac(N, D), New, X)
 => New = Flags,
    X = display(frac(N, D)).

test :- test(dfrac(1, pi)).

%
% Square root
%
ml(Flags, sqrt(A), M)
 => ml(Flags, A, X),
    M = msqrt(X).

prec(_Flags, sqrt(_), Prec)
 => current(P, xfy, ^),
    Prec is P + 1.

test :- test(sqrt(2)).
test :- test(sqrt(2)^2).

%
% Sum over index
%
ml(Flags, sum(I, From, To, A), M)
 => ml(Flags, I = From, XFrom),
    ml(Flags, To, XTo),
    ml(Flags, A, X),
    M = mrow([munderover([mo(&(sum)), XFrom, XTo]), X]).

paren(Flags, sum(_, _, _, A), Paren)
 => paren(Flags, A, Paren).

prec(_Flags, sum(_, _, _, _), Prec)
 => current(Prec, yfx, +).

test :- test(sum(i, 1, 10, i)).

%
% Intgrate over range
%
ml(Flags, integrate(Fn, From, To, DX), M)
 => ml(Flags, Fn, XFn),
    ml(Flags, From, XFrom),
    ml(Flags, To, XTo),
    ml(Flags, DX, XDX),
    ml(Flags, space, Space),
    M = mrow([munderover([mo(&(int)), XFrom, XTo]), XFn, Space, mi(d), XDX]).

paren(Flags, integrate(_, _, _, A), Paren)
 => paren(Flags, A, Paren).

prec(_Flags, integrate(_, _, _, _), Prec)
 => current(Prec, yfx, +).

test :- test(integrate(fn(f, [paren(x)]), 1, 10, x)).

%
% Large font ("displaystyle")
%
ml(Flags, display(A), M)
 => ml(Flags, A, X),
    M = mstyle(displaystyle(true), X).

type(Flags, display(A), Type)
 => type(Flags, A, Type).

%
% Decorations
%
ml(Flags, overline(A), M)
 => ml(Flags, A, X),
    M = mover(accent(true), [X, mo(&(macr))]).

paren(Flags, overline(A), Paren)
 => paren(Flags, A, Paren).

% Put overline(x)^2 in parentheses
prec(_Flags, overline(_), Prec)
 => current(Prec, yfx, *).

type(Flags, overline(A), Type)
 => type(Flags, A, Type).

test :- test(overline('D')).

%
% Cancel out
%
ml(Flags, cancel(A), M)
 => ml(Flags, A, X),
    M = menclose(notation(updiagonalstrike), X).

paren(Flags, cancel(A), Paren)
 => paren(Flags, A, Paren).

prec(Flags, cancel(A), Prec)
 => prec(Flags, A, Prec).

type(Flags, cancel(A), Type)
 => type(Flags, A, Type).

test :- test(cancel('D')).

%
% Box
%
ml(Flags, box(A), M)
 => ml(Flags, A, X),
    M = menclose(notation(roundedbox), X).

paren(Flags, box(A), Paren)
 => paren(Flags, A, Paren).

prec(Flags, box(A), Prec)
 => prec(Flags, A, Prec).

type(Flags, box(A), Type)
 => type(Flags, A, Type).

test :- test(box('D')).

%
% Underbrace
%
ml(Flags, underbrace(A, U), M)
 => ml(Flags, A, X),
    ml(Flags, U, Y),
    M = munder([munder(accentunder(true),
                  [Y, mo(stretchy(true), &('UnderBrace'))]), X]).

paren(Flags, underbrace(A, _), Paren)
 => paren(Flags, A, Paren).

prec(Flags, underbrace(A, _), Prec)
 => prec(Flags, A, Prec).

type(Flags, underbrace(A, _), Type)
 => type(Flags, A, Type).

test :- test(underbrace('D', u)).

%
% Mistakes
%
math(Flags, omit_left(Expr), New, M),
    option(error(ignore), Flags, highlight)
 => Flags = New,
    M = Expr.

math(Flags, omit_left(Expr), New, M),
    option(error(fix), Flags, highlight),
    Expr =.. [Op, L, R]
 => Flags = New,
    M = list(space, [box(list(space, [L, sign(Op)])), R]).

math(Flags, omit_left(Expr), New, M),
    option(error(highlight), Flags, highlight),
    Expr =.. [Op, L, R]
 => Flags = New,
    M = list(space, [cancel(list(space, [L, sign(Op)])), R]).

math(Flags, omit_right(Expr), New, M),
    option(error(ignore), Flags, highlight)
 => Flags = New,
    M = Expr.

math(Flags, omit_right(Expr), New, M),
    option(error(fix), Flags, highlight),
    Expr =.. [Op, L, R]
 => Flags = New,
    M = list(space, [L, box(list(space, [sign(Op), R]))]).

math(Flags, omit_right(Expr), New, M),
    option(error(highlight), Flags, highlight),
    Expr =.. [Op, L, R]
 => Flags = New,
    M = list(space, [L, cancel(list(space, [sign(Op), R]))]).

math(Flags, instead(_Wrong, Correct), New, M),
    option(error(ignore), Flags, highlight)
 => Flags = New,
    M = Correct.

math(Flags, instead(_Wrong, Correct), New, M),
    option(error(fix), Flags, highlight)
 => Flags = New,
    M = box(Correct).

math(Flags, instead(Wrong, Correct), New, M),
    option(error(highlight), Flags, highlight)
 => Flags = New,
    M = underbrace(list(space, ["instead of", Correct]), Wrong).

test :- test(dfrac(omit_right(overline('D') - mu),
                   sub(s, 'D') / sqrt('N'))).

test :- test(dfrac(overline('D') - mu,
                   sub(s, 'D') / instead('N', sqrt('N')))).

%
% Expert and buggy rules
%
math(Flags, expert(Flags, _, B), New, X)
 => New = Flags,
    X = B.

math(Flags, buggy(Flags, _, B), New, X)
 => New = Flags,
    X = B.

%
% Binomial coefficient and distribution
%
ml(Flags, choose(N, K), M)
 => ml(Flags, N, X),
    ml(Flags, K, Y),
    M = mrow([mo('('), mfrac([linethickness(0)], [X, Y]), mo(')')]).

paren(_Flags, choose(_, _), Paren)
 => Paren = 1.

prec(_Flags, choose(_, _), Prec)
 => Prec = 0.

type(_Flags, choose(_, _), Type)
 => Type = paren.

test :- test(choose('N', k)).

test :- test(choose('N', k) * pi^k * (1 - pi)^('N' - k)).

% Density, distribution etc.
math(Flags, dbinom(K, N, Pi), New, X)
 => New = Flags,
    X = fn(sub('P', "Bi"), (['X' = K] ; [N, Pi])).

math(Flags, pbinom(K, N, Pi), New, X)
 => New = Flags,
    X = fn(sub('P', "Bi"), (['X' =< K] ; [N, Pi])).

math(Flags, upbinom(K, N, Pi), New, X)
 => New = Flags,
    X = fn(sub('P', "Bi"), (['X' >= K] ; [N, Pi])).

math(Flags, cbinom(Alpha, N, Pi, Tail, Dist), New, X)
 => New = Flags,
    X = fn(Tail, [fn(sub('P', "Bi"), ([Dist] ; [N, Pi])) =< Alpha]).

math(Flags, tail("upper"), New, X)
 => New = Flags,
    X = sub("argmin", k).

math(Flags, tail("lower"), New, X)
 => New = Flags,
    X = sub("argmax", k).

math(Flags, tail("upperdens"), New, X)
 => New = Flags,
    X = sub("argmin", k > 'N' * pi).

math(Flags, tail("lowerdens"), New, X)
 => New = Flags,
    X = sub("argmax", k < 'N' * pi).

math(Flags, dist("upper"), New, X)
 => New = Flags,
    X = ('X' >= k).

math(Flags, dist("lower"), New, X)
 => New = Flags,
    X = ('X' =< k).

math(Flags, dist("density"), New, X)
 => New = Flags,
    X = ('X' = k).

test :- test(dbinom(k, 'N', pi)).
test :- test(pbinom(k, 'N', pi)).
test :- test(upbinom(k, 'N', pi)).
test :- test(cbinom(alpha, 'N', pi, tail("lower"), dist("lower"))).
test :- test(cbinom(alpha, 'N', pi, tail("upper"), dist("upper"))).
test :- test(cbinom(alpha, 'N', pi, tail("lowerdens"), dist("density"))).
test :- test(cbinom(alpha, 'N', pi, tail("upperdens"), dist("density"))).

%
% Trigonometry
%
math(Flags, sin(Alpha), New, X)
 => New = Flags,
    X = fn("sin", [Alpha]).

math(Flags, cos(Alpha), New, X)
 => New = Flags,
    X = fn("cos", [Alpha]).

math(Flags, tan(Alpha), New, X)
 => New = Flags,
    X = fn("tan", [Alpha]).

%
% Functions like f(x) and f(x; a, b)
%
ml(Flags, fn(Name, (Args ; Params)), M)
 => ml(Flags, Name, F),
    ml(Flags, paren(list(sign(';'), [list(sign(','), Args), list(sign(','), Params)])), X),
    M = mrow([F, mo(&(af)), X]).

paren(Flags, fn(_Name, (Args ; Params)), Paren)
 => paren(Flags, list(sign(','), Args), X),
    paren(Flags, list(sign(','), Params), Y),
    Paren is max(X, Y).

prec(Flags, fn(_Name, (_Args ; _Params)), Prec)
 => prec(Flags, a * b, Prec).

type(_Flags, fn(_Name, (_Args ; _Params)), Type)
 => Type = paren.

ml(Flags, fn(Name, [Arg]), M),
    member(Name, ["sin", "cos", "tan"]),
    prec(Flags, Arg, P),
    P = 0
 => ml(Flags, Name, F),
    ml(Flags, Arg, X),
    M = mrow([F, mo(&(af)), X]).

ml(Flags, fn(Name, [Arg]), M)
 => ml(Flags, Name, F),
    ml(Flags, paren([Arg]), X),
    M = mrow([F, mo(&(af)), X]).

ml(Flags, fn(Name, Args), M)
 => ml(Flags, Name, F),
    ml(Flags, paren(list(sign(','), Args)), X),
    M = mrow([F, mo(&(af)), X]).

paren(Flags, fn(_Name, Args), Paren)
 => paren(Flags, list(sign(','), Args), Paren).

prec(_Flags, fn(_Name, _Args), Prec)
 => current(Prec, yfx, *).

type(_Flags, fn(_Name, _Args), Type)
 => Type = paren.

%
% Defaults
%
math(Flags, A, New, X)
 => A = X,
    New = Flags.

paren(Flags, A, Den),
    math(Flags, A, New, M),
    dif(Flags-A, New-M)
 => paren(New, M, Den).

paren(_, _, P) =>
    P = 0.

prec(Flags, A, Den),
    math(Flags, A, New, M),
    dif(Flags-A, New-M)
 => prec(New, M, Den).

prec(_, _, P) =>
    P = 0.

type(Flags, A, Type),
    math(Flags, A, New, M),
    dif(Flags-A, New-M)
 => type(New, M, Type).

type(_Flags, A, Type),
    compound(A)
 => Type = compound.

denoting(Flags, A, Den),
    math(Flags, A, New, M),
    dif(Flags-A, New-M)
 => denoting(New, M, Den).

denoting(Flags, Expression, Den),
    compound(Expression)
 => compound_name_arguments(Expression, _, Arguments),
    maplist(denoting(Flags), Arguments, List),
    append(List, Den).

% If everything fails, there is no abbreviation
denoting(_Flags, _, Den)
 => Den = [].

% Precedence
current(Prec, Fix, Op),
    atom(Op)
 => current_op(Prec, Fix, Op).

%
% Tests
%
test(A) :-
    writeln(A),
    mathml([], A, M),
    html(math(M)).
