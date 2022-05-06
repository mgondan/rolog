:- use_module(library(dcg/basics)).
:- discontiguous mjax//2.

r2mathjax(A, X) :-
    mjax(A, X).

% Atomic things (will probably be changed)
mjax(alpha, f(paren=0, _)) -->
    "\\alpha", !.

mjax(mu, f(paren=0, _)) -->
    "\\mu", !.

mjax(sigma, f(paren=0, _)) -->
    "\\sigma", !.

mjax(pi, f(paren=0, _)) -->
    "\\pi", !.

mjax(X, f(paren=0, _)) -->
    {atom(X)},
    !, atom(X).

mjax(-1.0Inf, f(paren=0, _)) -->
    !, "-\\infty".

mjax(1.0Inf, f(paren=0, _)) -->
    !, "\\infty".

mjax(X, f(paren=0, _)) -->
    {number(X)},
    !, number(X).

mjax(X, f(paren=0, _)) -->
    {string(X), atom_string(A, X)},
    !, "\\mathrm{", atom(A), "}".

% Parentheses
mjax(brace(X), f(paren=P, F)) -->
    "{", mjax(X, f(paren=P, F)), "}".

mjax(paren(X), f(paren=1, F)) -->
    "\\left(", mjax(X, f(paren=0, F)), "\\right)".

mjax(paren(X), f(paren=2, F)) -->
    "\\left[", mjax(X, f(paren=1, F)), "\\right]".

mjax(paren(X), f(paren=P, F)) -->
    "\\left\\{", mjax(X, f(paren=Q, F)), "\\right\\}",
    {Q >= 2, P is Q+1}.

% Function arguments
mjax((A, B), f(paren=P, F)) -->
    mjax(A, f(paren=Q, F)), ", ", mjax(B, f(paren=R, F)),
    {P is max(Q, R)}.

mjax((A | B), f(paren=P, F)) -->
    mjax(A, f(paren=Q, F)), "\\,\\middle|\\,", mjax(B, f(paren=R, F)),
    {P is max(Q, R)}.

mjax((A ; B), f(paren=P, F)) -->
    mjax(A, f(paren=Q, F)), ";", mjax(B, f(paren=R, F)),
    {P is max(Q, R)}.

mjax((A : B), f(paren=P, F)) -->
    mjax(A, f(paren=Q, F)), ":", mjax(B, f(paren=R, F)),
    {P is max(Q, R)}.

mjax(A // B, f(paren=P, F)) -->
    mjax(A, f(paren=Q, F)), "\\ ", mjax(B, f(paren=R, F)),
    {P is max(Q, R)}.

% Signs (right/2 checks if parentheses are needed)
mjax(-X, P) -->
    "-", mjax(right(-, X), P).

mjax(+X, P) -->
    "+", mjax(right(+, X), P).

% render [sin(x)]^2 as sin^2 x
mjax(sin(A)^B, f(paren=P, F)) -->
    mjax('\\sin'^B, f(_, F)), mjax(left(A, '.'), f(paren=P, F)), !.

mjax(cos(A)^B, f(paren=P, F)) -->
    mjax('\\cos'^B, f(_, F)), mjax(left(A, '.'), f(paren=P, F)), !.

mjax(tan(A)^B, f(paren=P, F)) -->
    mjax('\\tan'^B, f(_, F)), mjax(left(A, '.'), f(paren=P, F)), !.

% render sin(x) as "sin x" if x is "simple", otherwise "sin(x)"
mjax(sin(X), f(paren=0, F)) -->
    {atomic(X)},
    "\\sin", mjax(brace(X), f(paren=0, F)), !.

mjax(cos(X), f(paren=0, F)) -->
    {atomic(X)},
    "\\cos", mjax(brace(X), f(paren=0, F)), !.

mjax(tan(X), f(paren=0, F)) -->
    {atomic(X)},
    "\\tan", mjax(brace(X), f(paren=0, F)), !.

% more alternatives
mjax(sin(X), P) -->
    {compound(X), compound_name_arity(X, Name, 2), current_op(_, _Fix, Name)},
    !, "\\sin", mjax(right(/, X), P).

mjax(cos(X), P) -->
    {compound(X), compound_name_arity(X, Name, 2), current_op(_, _Fix, Name)},
    !, "\\cos", mjax(brace(right(/, X)), P).

mjax(tan(X), P) -->
    {compound(X), compound_name_arity(X, Name, 2), current_op(_, _Fix, Name)},
    !, "\\tan", mjax(brace(right(/, X)), P).

% with parentheses
mjax(sin(X), P) --> "\\sin", mjax(paren(X), P), !.
mjax(cos(X), P) --> "\\cos", mjax(paren(X), P), !.
mjax(tan(X), P) --> "\\tan", mjax(paren(X), P), !.

% Equality & Co.
mjax(A = B, f(paren=P, F)) -->
    mjax(left(A, =), f(paren=Q, F)), " = ", mjax(right(=, B), f(paren=R, F)),
    {P is max(Q, R)}.

mjax(A \= B, f(paren=P, F)) -->
    mjax(left(A, \=), f(paren=Q, F)), "\\neq ", mjax(right(\=, B), f(paren=R, F)),
    {P is max(Q, R)}.

mjax(A < B, f(paren=P, F)) -->
    mjax(left(A, <), f(paren=Q, F)), " < ", mjax(right(<, B), f(paren=R, F)),
    {P is max(Q, R)}.

mjax(A > B, f(paren=P, F)) -->
    mjax(left(A, >), f(paren=Q, F)), " > ", mjax(right(>, B), f(paren=R, F)),
    {P is max(Q, R)}.

mjax(A =< B, f(paren=P, F)) -->
    mjax(left(A, =<), f(paren=Q, F)), "\\leq ", mjax(right(=<, B), f(paren=R, F)),
    {P is max(Q, R)}.

mjax(A >= B, f(paren=P, F)) -->
    mjax(left(A, >=), f(paren=Q, F)), "\\geq ", mjax(right(>=, B), f(paren=R, F)),
    {P is max(Q, R)}.

% sum
mjax(A + B, f(paren=P, F)) -->
    mjax(left(A, +), f(paren=Q, F)), " + ", mjax(right(+, B), f(paren=R, F)),
    {P is max(Q, R)}.

% difference, insert parentheses if needed [e.g., for a - (b + c)]
mjax(A - B, f(paren=P, F)) -->
    mjax(left(A, -), f(paren=Q, F)), " - ", mjax(right(-, B), f(paren=R, F)),
    {P is max(Q, R)}.

% dot product
mjax(A * B, P) -->
    "{", mjax(left(A, *), f(paren=0, F)), "}",
    "{", mjax(right(*, B), f(paren=0, F)), "}",
    !, { P = f(paren=0, F) }.

% dot product
mjax(A * B, f(paren=P, F)) -->
    mjax(left(A, *), f(paren=Q, F)), 
    " \\cdot ", 
    mjax(right(*, B), f(paren=R, F)),
    {P is max(Q, R)}.

% ratio
mjax(A / B, f(paren=P, F)) -->
    mjax(left(A, /), f(paren=Q, F)), " / ", mjax(right(/, B), f(paren=R, F)),
    {P is max(Q, R)}.

% power, no parenthesis needed for exponent
mjax(A^B, f(paren=P, F)) -->
    mjax(left(A, ^), f(paren=P, F)), "^", mjax(brace(B), f(paren=_, F)).

% subscript
mjax(sub(X, S), f(paren=P, F)) -->
    mjax(brace(X), f(paren=P, F)), "_", mjax(brace(S), f(paren=_, F)).

% proper fractions
mjax(frac(A, B), f(paren=0, F)) -->
    "\\frac", mjax(brace(A), f(paren=_, F)), mjax(brace(B), f(paren=_, F)).

mjax(dfrac(A, B), f(paren=0, F)) -->
    "\\dfrac", mjax(brace(A), f(paren=_, F)), mjax(brace(B), f(paren=_, F)).

% other stuff
mjax(sqrt(X), f(paren=0, F)) -->
    "\\sqrt", mjax(brace(X), f(paren=_, F)).

mjax(function(sin, Args), P) -->
    !, mjax(sin(Args), P).

mjax(function(cos, Args), P) -->
    !, mjax(cos(Args), P).

mjax(function(tan, Args), P) -->
    !, mjax(tan(Args), P).

mjax(function(Name, Args), f(paren=P, F)) -->
    mjax(Name, f(paren=_, F)), mjax(paren(Args), f(paren=P, F)).

mjax(dbinom(K, N, Pi), P) -->
    mjax(function(sub('P', "Bi"), (('X' = K) | (N, Pi))), P).

mjax(pbinom(K, N, Pi), P) -->
    mjax(function(sub('P', "Bi"), (('X' >= K) | (N, Pi))), P).

mjax(ubinom(K, N, Pi), P) -->
    mjax(function(sub('P', "Bi"), (('X' =< K) | (N, Pi))), P).

mjax(choose(N, K), f(paren=1, F)) -->
    mjax(brace(N), f(paren=_, F)), "\\choose", mjax(brace(K), f(paren=_, F)).

% strange name, don't ask
mjax(bernoulli(K, N, Pi), P) -->
    mjax(Pi^K * (1 - Pi)^(N - K), P).

:- op(180, xf, factorial).
mjax(factorial(X), P) -->
    mjax(left(X, factorial), P), "!".

% Mistakes
mjax(omit0(X), f(paren=P, error=show)) -->
    mjax(brace(cancel(X)), f(paren=P, error=show)).

mjax(omit0(X), f(paren=P, error=correct)) -->
    mjax(green(X), f(paren=P, error=correct)).

mjax(omit0(_), f(paren=P, error=accept)) -->
    mjax(red(0), f(paren=P, error=accept)).

mjax(omit1(X), f(paren=P, error=show)) -->
    mjax(brace(cancel(X)), f(paren=P, error=show)).

mjax(omit1(X), f(paren=P, error=correct)) -->
    mjax(green(X), f(paren=P, error=correct)).

mjax(omit1(_), f(paren=P, error=accept)) -->
    mjax(red(1), f(paren=P, error=accept)).

mjax(instead_of(Instead, Of), f(paren=P, error=show)) -->
    mjax(sub(underbrace(red(Instead)), ("instead of" // green(Of))), f(paren=P, error=show)).

mjax(instead_of(Instead, _), f(paren=P, error=accept)) -->
    mjax(red(Instead), f(paren=P, error=accept)).

mjax(instead_of(_Instead, Of), f(paren=P, error=correct)) -->
    mjax(green(Of), f(paren=P, error=correct)).

% Decorations
mjax(underbrace(X), P) -->
    "\\underbrace", mjax(brace(X), P).

mjax(cancel(X), f(paren=P, F)) -->
    % "\\require", mjax(brace(cancel), f(paren=_, F)), % check if needed
    mjax(red(cancelled(black(X))), f(paren=P, F)).

mjax(red(X), P) -->
    mjax(color(red, X), P).

mjax(green(X), P) -->
    mjax(color(green, X), P).

mjax(black(X), P) -->
    mjax(color(black, X), P).

mjax(color(Col, X), f(paren=P, F)) -->
    "\\color", mjax(brace(Col), f(paren=_, F)), mjax(brace(X), f(paren=P, F)).

mjax(cancelled(X), P) -->
    "\\cancel", mjax(brace(X), P).

% Check parentheses
mjax(left(instead_of(X, Of), Op), P) -->
    !, mjax(instead_of(left(X, Op), Of), P).

mjax(left(X, Op), P) -->
    {
        compound(X),
        compound_name_arity(X, Name, 2), current_op(Prec, yfx, Name),
        current_op(Op_Prec, Op_Fix, Op), member(Op_Fix, [yfx, xf, xfy]),
        Prec > Op_Prec
    }, !,
    mjax(paren(X), P).

mjax(left(X, Op), P) -->
    {
        compound(X),
        compound_name_arity(X, Name, 2), current_op(Prec, xfy, Name),
        current_op(Op_Prec, Op_Fix, Op), member(Op_Fix, [yfx, xf, xfy]),
        Prec >= Op_Prec
    }, !,
    mjax(paren(X), P).

% Treat plus sign as sum
mjax(left(+X, Op), P) -->
    {
        current_op(Prec, yfx, +),
        current_op(Op_Prec, Op_Fix, Op), member(Op_Fix, [yfx, xf, xfy]),
        Prec > Op_Prec
    }, !,
    mjax(paren(+X), P).

% Treat negative sign as minus
mjax(left(-X, Op), P) -->
    {
        current_op(Prec, yfx, -),
        current_op(Op_Prec, Op_Fix, Op), member(Op_Fix, [yfx, xf, xfy]),
        Prec > Op_Prec
    }, !,
    mjax(paren(-X), P).

% Avoid ambiguous cos x/2
mjax(left(sin(X), /), f(paren=1, _)) -->
    !, mjax(paren(sin(X)), f(paren=1, _)).

mjax(left(cos(X), /), f(paren=1, _)) -->
    !, mjax(paren(cos(X)), f(paren=1, _)).

mjax(left(tan(X), /), f(paren=1, _)) -->
    !, mjax(paren(tan(X)), f(paren=1, _)).

% Integral
mjax($(integrate(Fn, Lower, Upper), value), P) -->
    !, mjax(integrate(Fn, Lower, Upper), P).

% with named arguments
mjax(integrate(f=Fn, lower=Lower, upper=Upper), P) -->
    !, mjax(integrate(Fn, Lower, Upper), P).

% No argument names
mjax(integrate(Fn, Lower, Upper), P) -->
    !, 
    { r_eval('['(formalArgs(args(Fn)), 1), Arg1),
      atom_string(DX, Arg1)
    }, mjax(integrate(Fn, Lower, Upper, DX), P).

% Internal
mjax(integrate(Fn, From, To, DX), P) -->
    !, "\\int_{", mjax(From, _), "}",
    "^{", mjax(To, _), "}",
    "{", mjax(function(Fn, DX), P), "}",
    "\\,{", mjax(d*DX, _), "}".

% Negative numbers
mjax(left(X, Op), P) -->
    { number(X), X < 0, A is abs(X) }, !,
    mjax(left(-A, Op), P).

% Default
mjax(left(X, _), P) -->
    mjax(brace(X), P).

mjax(right(instead_of(X, Of), Op), P) -->
    !, mjax(instead_of(right(X, Op), Of), P).

mjax(right(Op, X), P) -->
    {
        compound(X), compound_name_arity(X, Name, 2),
        current_op(Prec, yfx, Name),
        current_op(Op_Prec, Op_Fix, Op), member(Op_Fix, [yfx, xf, xfy]),
        Prec > Op_Prec
    }, !,
    mjax(paren(X), P).

% a - (b + c)
mjax(right(-, X), P) -->
    {
        compound(X), compound_name_arity(X, Name, 2), current_op(Prec, yfx, Name),
        current_op(Prec, yfx, -)
    }, !,
    mjax(paren(X), P).

% a / (b * c)
mjax(right(/, X), P) -->
    {
        compound(X), compound_name_arity(X, Name, 2), current_op(Prec, yfx, Name),
        current_op(Prec, yfx, /)
    }, !,
    mjax(paren(X), P).

% Treat plus sign as sum
mjax(right(Op, +X), P) -->
    {
        current_op(Op_Prec, Op_Fix, Op), member(Op_Fix, [yfx, xf, xfy]),
        current_op(Prec, yfx, +), Prec > Op_Prec
    }, !,
    mjax(paren(+X), P).

% Treat negative sign as minus
mjax(right(Op, -X), P) -->
    {
        current_op(Op_Prec, Op_Fix, Op), member(Op_Fix, [yfx, xf, xfy]),
        current_op(Prec, yfx, -), Prec > Op_Prec
    }, !,
    mjax(paren(-X), P).

% negative numbers
mjax(right(Op, X), P) -->
    { number(X), X < 0, A is abs(X) }, !,
    mjax(right(Op, -A), P).

% default
mjax(right(_, X), P) -->
    mjax(brace(X), P).

% interface & examples
mjax(M, S) :-
    mjax(M, _, J, []),
    string_codes(S, J).

example(S, M) :- S = dfrac('M' - omit0(mu), instead_of(sigma^2, s^2) / omit1(sqrt('N'))), mjax(S, M).
example(S, M) :- S = cos(x)/2, mjax(S, M).
example(S, M) :- S = sin(cos(x)), mjax(S, M).
example(S, M) :- S = paren(dfrac(1, paren(paren(paren(x))))), mjax(S, M).
example(S, M) :- S = dfrac('M' - mu, instead_of(sigma^2, s^2) / sqrt('N')), mjax(S, M).
example(S, M) :- S = sqrt('N'), mjax(S, M).
example(S, M) :- S = dfrac(factorial(n), factorial(x)*cancel(factorial(n-x))), mjax(S, M).
example(S, M) :- S = dfrac(factorial(n), factorial(x)*factorial(n-x)), mjax(S, M).
example(S, M) :- S = dfrac((x_i - overline(x))^2, n-1), mjax(S, M).
example(S, M) :- S = choose(n, x) * pi^x * (1-pi)^(n-x), mjax(S, M).
example(S, M) :- S = bernoulli(x, size, prob), mjax(S, M).
example(S, M) :- S = dbinom(x, size, prob), mjax(S, M).
example(S, M) :- S = a - (b - (c - d)), mjax(S, M).
example(S, M) :- S = sin(b), mjax(S, M).
example(S, M) :- S = sin(b^2), mjax(S, M).
example(S, M) :- S = sin(b)^2, mjax(S, M).
example(S, M) :- S = sin(b^2)^2, mjax(S, M).
example(S, M) :- S = sin(b / c)^2, mjax(S, M).
example(S, M) :- S = sin(b / c), mjax(S, M).
example(S, M) :- S = a / (b / c), mjax(S, M).
example(S, M) :- S = a / b * c, mjax(S, M).
example(S, M) :- S = a - (b - c), mjax(S, M).
example(S, M) :- S = a - (b + c), mjax(S, M).
example(S, M) :- S = a + (b + c), mjax(S, M).
example(S, M) :- S = a + (b - c), mjax(S, M).
example(S, M) :- S = a - b - c, mjax(S, M).
example(S, M) :- S = (-1) * (-1), mjax(S, M).
example(S, M) :- S = sin(alpha) * cos(alpha), mjax(S, M).
example(S, M) :- S = a * (c + d), mjax(S, M).
example(S, M) :- S = (a + b) * c, mjax(S, M).
example(S, M) :- S = (a + b) * (c + d), mjax(S, M).
example(S, M) :- S = function(f, ((a, b) ; (c, d))), mjax(S, M).
