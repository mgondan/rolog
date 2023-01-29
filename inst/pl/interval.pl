% Interval arithmetic in Prolog
:- module(interval, [int/2, op(150, xfx, ...)]).

:- set_prolog_flag(float_overflow, infinity).
:- discontiguous int/2.
:- discontiguous interval/2.
:- multifile hook/2.

% Allows for external definitions
int(Expr, Res),
    hook(Expr, Hook)
 => int(Hook, Res).

%
% Hickey Figure 1
%
mixed(L, U) :-
    L < 0,
    U > 0.

zero(0.0, 0.0).

positive(L, U) :-
    L >= 0,
    U > 0.

zeropos(0.0, U) :-
    U > 0.

strictpos(L, _) :-
    L > 0.

negative(L, U) :-
    L < 0,
    U =< 0.

zeroneg(L, 0.0) :-
    L < 0.

strictneg(_, U) :-
    U < 0.

%
% Convert to interval
%
int(X, Res),
    atomic(X)
 => L is X,
    U is X,
    Res = L ... U.

int(X ... Y, Res)
 => interval(X ... Y, Res).

% compatible with atoms like pi
interval(A ... B, Res) :-
    !,
    L is A,
    U is B,
    Res = L ... U.

%
% Equality = overlapping
%
int(X =@= Y, Res)
 => int(X, A ... B),
    int(Y, C ... D),
    L is max(A, C),
    U is min(B, D),
    L =< U,
    Res = L ... U.

%
% Hickey, Theorem 4
%
int(X + Y, Res)
 => int(X, ResX),
    int(Y, ResY),
    interval(ResX + ResY, Res).

interval(A...B + C...D, Res) :-
    L is A + C,
    U is B + D,
    Res = L ... U.

%
% Hickey, Theorem 4
%
int(X - Y, Res)
 => int(X, ResX),
    int(Y, ResY),
    interval(ResX - ResY, Res).

interval(A...B - C...D, Res) :-
    L is A - D,
    U is B - C,
    Res = L ... U.

%
% Hickey Theorem 6 and Figure 3
%
int(X * Y, Res)
 => int(X, ResX),
    int(Y, ResY),
    interval(ResX * ResY, Res).

%
% Hickey, Figure 3 (last rows first)
%
interval(A ... B * C ... D, Res) :-
    once(zero(A, B); zero(C, D)),
    !,
    Res = 0.0 ... 0.0.

% P * P
interval(A ... B * C ... D, Res) :-
    positive(A, B),
    positive(C, D),
    !,
    L is A * C,
    U is B * D,
    Res = L ... U.

% P * M
interval(A ... B * C ... D, Res) :-
    positive(A, B),
    mixed(C, D),
    !,
    L is B * C,
    U is B * D,
    Res = L ... U.

% P * N
interval(A ... B * C ... D, Res) :-
    positive(A, B),
    negative(C, D),
    !,
    L is B * C,
    U is A * D,
    Res = L ... U.

% M * P
interval(A ... B * C ... D, Res) :-
    mixed(A, B),
    positive(C, D),
    !,
    L is A * D,
    U is B * D,
    Res = L ... U.

% M * M
interval(A ... B * C ... D, Res) :-
    mixed(A, B),
    mixed(C, D),
    !,
    L is min(A * D, B * C),
    U is max(A * C, B * D),
    Res = L ... U.

% M * N
interval(A ... B * C ... D, Res) :-
    mixed(A, B),
    negative(C, D),
    !,
    L is B * C,
    U is A * C,
    Res = L ... U.

% N * P
interval(A ... B * C ... D, Res) :-
    negative(A, B),
    positive(C, D),
    !,
    L is A * D,
    U is B * C,
    Res = L ... U.

% N * M
interval(A ... B * C ... D, Res) :-
    negative(A, B),
    mixed(C, D),
    !,
    L is A * D,
    U is A * C,
    Res = L ... U.

% N * N
interval(A ... B * C ... D, Res) :-
    negative(A, B),
    negative(C, D),
    !,
    L is B * D,
    U is A * C,
    Res = L ... U.

%
% Hickey Theorem 8 and Figure 4
%
int(X / Y, Res)
 => int(X, ResX),
    int(Y, ResY),
    interval(ResX / ResY, Res).

% P1 / P (special case, then general case)
interval(A ... B / 0.0 ... D, Res) :-
    strictpos(A, B),
    positive(0.0, D),
    !,
    L is A / D,
    U is 1.0Inf,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    strictpos(A, B),
    positive(C, D),
    !,
    L is A / D,
    U is B / C,
    Res = L ... U.

% P0 / P
interval(A ... B / 0.0 ... D, Res) :-
    zeropos(A, B),
    positive(0.0, D),
    !,
    L is 0.0,
    U is 1.0Inf,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    zeropos(A, B),
    positive(C, D),
    !,
    L is 0.0,
    U is B / C,
    Res = L ... U.

% M / P
interval(A ... B / 0.0 ... D, Res) :-
    mixed(A, B),
    positive(0.0, D),
    !,
    L is -1.0Inf,
    U is 1.0Inf,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    mixed(A, B),
    positive(C, D),
    !,
    L is A / C,
    U is B / C,
    Res = L ... U.

% N0 / P
interval(A ... B / 0.0 ... D, Res) :-
    zeroneg(A, B),
    positive(0.0, D),
    !,
    L is -1.0Inf,
    U is 0.0,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    zeroneg(A, B),
    positive(C, D),
    !,
    L is A / C,
    U is 0.0,
    Res = L ... U.

% N1 / P
interval(A ... B / 0.0 ... D, Res) :-
    strictneg(A, B),
    positive(0.0, D),
    !,
    L is -1.0Inf,
    U is B / D,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    strictneg(A, B),
    positive(C, D),
    !,
    L is A / C,
    U is B / D,
    Res = L ... U.

% P1 / M (2 solutions)
interval(A ... B / C ... D, Res) :-
    strictpos(A, B),
    mixed(C, D),
    L is -1.0Inf,
    U is A / C,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    strictpos(A, B),
    mixed(C, D),
    !,
    L is A / D,
    U is 1.0Inf,
    Res = L ... U.

% P0 / M
interval(A ... B / C ... D, Res) :-
    zeropos(A, B),
    mixed(C, D),
    !,
    L is -1.0Inf,
    U is 1.0Inf,
    Res = L ... U.

% M / M
interval(A ... B / C ... D, Res) :-
    mixed(A, B),
    mixed(C, D),
    !,
    L is -1.0Inf,
    U is 1.0Inf,
    Res = L ... U.

% N0 / M
interval(A ... B / C ... D, Res) :-
    zeroneg(A, B),
    mixed(C, D),
    !,
    L is -1.0Inf,
    U is 1.0Inf,
    Res = L ... U.

% N1 / M (2 solutions)
interval(A ... B / C ... D, Res) :-
    strictneg(A, B),
    mixed(C, D),
    L is -1.0Inf,
    U is B / D,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    strictneg(A, B),
    mixed(C, D),
    !,
    L is B / C,
    U is 1.0Inf,
    Res = L ... U.

% P1 / N
interval(A ... B / C ... 0.0, Res) :-
    strictpos(A, B),
    negative(C, 0.0),
    !,
    L is -1.0Inf,
    U is A / C,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    strictpos(A, B),
    negative(C, D),
    !,
    L is B / D,
    U is A / C,
    Res = L ... U.

% P0 / N
interval(A ... B / C ... 0.0, Res) :-
    zeropos(A, B),
    negative(C, 0.0),
    !,
    L is -1.0Inf,
    U is 0.0,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    zeropos(A, B),
    negative(C, D),
    !,
    L is B / D,
    U is 0.0,
    Res = L ... U.

% M / N
interval(A ... B / C ... 0.0, Res) :-
    mixed(A, B),
    negative(C, 0.0),
    !,
    L is -1.0Inf,
    U is 1.0Inf,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    mixed(A, B),
    negative(C, D),
    !,
    L is B / D,
    U is A / D,
    Res = L ... U.

% N0 / N
interval(A ... B / C ... 0.0, Res) :-
    zeroneg(A, B),
    negative(C, 0.0),
    !,
    L is 0.0,
    U is 1.0Inf,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    zeroneg(A, B),
    negative(C, D),
    !,
    L is 0.0,
    U is A / D,
    Res = L ... U.

% N1 / N
interval(A ... B / C ... 0.0, Res) :-
    strictneg(A, B),
    negative(C, 0.0),
    !,
    L is B / C,
    U is 1.0Inf,
    Res = L ... U.

interval(A ... B / C ... D, Res) :-
    strictneg(A, B),
    negative(C, D),
    !,
    L is B / C,
    U is A / D,
    Res = L ... U.

%
% A simple macro for fractions that just invokes '/'/2
%
int(frac(X, Y), Res)
 => int(X / Y, Res).

int(dfrac(Num, Den), Res)
 => int(frac(Num, Den), Res).

%
% Binomial density and distribution. This illustrates the use of R functions
% that behave monotonically in their arguments.
%
% If lower.tail=TRUE, pbinom is monotonically decreasing in Prob
int(pbinom(Q, Size, Prob, Tail, Log), Res),
    r_eval(Tail, true)
 => int(Prob, ProbL ... ProbU),
    r_eval(pbinom(Q, Size, ##(ProbU, ProbL), Tail, Log), ##(L, U)),
    Res = L ... U.

int(pbinom(Q, Size, Prob, Tail, Log), Res)
 => int(Prob, ProbL ... ProbU),
    r_eval(pbinom(Q, Size, ##(ProbL, ProbU), Tail, Log), ##(L, U)),
    Res = L ... U.

% For X < Prob*Size, dbinom is monotonically decreasing in Prob, above that
% value, it is monotonically increasing.
int(dbinom(X, Size, Prob, Log), Res)
 => r_eval(Size, SizeR),
    int(Prob, ProbL ... ProbU),
    (   X < ProbL*SizeR
     -> r_eval(dbinom(X, Size, ##(ProbU, ProbL), Log), ##(L, U))
      ; X > ProbU*SizeR
     -> r_eval(dbinom(X, Size, ##(ProbL, ProbU), Log), ##(L, U))
      ; r_eval(X/Size, ProbM),
        r_eval(dbinom(X, Size, ##(ProbL, ProbM, ProbU), Log), ##(B1, B2, B3)),
        min_list([B1, B2, B3], L),
        max_list([B1, B2, B3], U)
    ),
    Res = L ... U.

%
% Helper functions
%
bound(L ... _, B) :-
    B = L.

bound(_ ... U, B) :-
    !,
    B = U.

bound(X, B) :-
    B = X.

%
% General functions that do not account for intervals.
%
% Examples
% * int(sqrt(0.1 ... 0.2), X)
% * int(sqrt(0.1), X)
%
% Evaluate interval for the arguments first, and then (blindly) apply the
% function to the lower and upper bound. Obviously, this only works for
% functions that behave monotonically in all their arguments.
%
int(X, Res),
    compound(X)
 => compound_name_arguments(X, Name, Arguments),
    maplist(int, Arguments, Results),
    findall(R,
      ( maplist(bound, Results, Bounds),
        compound_name_arguments(New, Name, Bounds),
        R is New
      ), List),
    min_list(List, L),
    max_list(List, U),
    ( length(List, 1)
     -> [Res] = List
     ;  Res = L ... U
    ).
