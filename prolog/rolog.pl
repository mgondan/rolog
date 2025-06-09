:- module(rolog, 
  [
    r_init/0,
    r_call/1,
    r_eval/2,
    op(600, xfy, ::),
    op(800, xfx, <-),
    op(800, fx, <-),
    op(100, yf, []),
    '<-'/1,
    '<-'/2
  ]).

:- (   current_prolog_flag(windows, true),
       getenv('R_HOME', RHOME)
   ->  directory_file_path(RHOME, bin, BIN),
       directory_file_path(BIN, x64, X64),
       win_add_dll_directory(X64)
   ;   true
   ),
   use_foreign_library(foreign(rolog)).

:- use_module(library(terms)).

r_call(Expr) :-
    pl2r_(Expr, R),
    with_mutex(rolog, r_eval_(R)).

r_eval(X, Y) :-
    pl2r_(X, R),
    with_mutex(rolog, r_eval_(R, Y)).

pl2r_('::'(Namespace, Compound), X)
 => term_string(Namespace, Ns),
    compound_name_arguments(Compound, Name, Arguments),
    pl2r_('do.call'($(getNamespace(Ns), Name), Arguments), X).

pl2r_(A =< B, X)
 => pl2r_('<='(A, B), X).

pl2r_(A[B], X)
 => pl2r_('['(A, B), X).

pl2r_({}, X)
 => X = [].
 
pl2r_({A; B}, X)
 => pl2r_curly({A; B}, C),
    S =.. [';' | C],
    X = '{'(S).

pl2r_({A}, X)
 => pl2r_(A, C),
    X = '{'(C).

pl2r_(Hash, X),
    compound(Hash),
    compound_name_arguments(Hash, #, Args)
 => compound_name_arguments(C, c, Args),
    pl2r_(C, X).

pl2r_(A, X),
    compound(A)
 => mapargs(pl2r_, A, X).

pl2r_(A, X)
 => A = X.
    
pl2r_curly({A; B}, X)
 => pl2r_(A, H),
    pl2r_curly({B}, T),
    X = [H | T].

pl2r_curly({A}, X)
 => pl2r_(A, H),
    X = [H].

<-(Expr) :-
    r_call(Expr).

% Assign variable in R
<-(Var, Expr) :-
    atom(Var),
    !,
    r_call('<-'(Var, Expr)).

% Assign variable in Prolog
<-(Res, Expr) :-
    r_eval(Expr, Res).

:- initialization(r_init).

% Windows: R not in PATH
r_init :-
    current_prolog_flag(windows, true),
    \+ process_which(path('R'), _),
    !,
    resource_error('R.exe not in PATH').

r_init :-
    current_prolog_flag(windows, true),
    \+ getenv('R_HOME', _),
    !,
    resource_error('R_HOME not set.').

r_init :-
    r_init_.
