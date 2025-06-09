:- module(rolog_threads, [r_init/1, r_eval/3, r_submit/3, r_close/1]).

:- reexport(library(rolog)).
:- use_module(library(broadcast)).

r_init(Session) :-
    <- library('RSclient'),
    Session <- 'RS.connect'().

r_eval(Session, Expr, Result) :-
    idle(Session),
    Result <- 'RS.eval'(Session, Expr, wait=true).

r_submit(Session, Alias, Expr) :-
    idle(Session),
    assert(lock(Session)),
    <- 'RS.eval'(Session, Expr, wait=false),
    thread_create(r_collect(Session, Alias, Expr), _).

r_collect(Session, Alias, Expr) :-
    Result <- 'RS.collect'(Session, timeout=0),
    (   Result = []
     -> sleep(0.1),
        r_collect(Session, Alias, Expr)
     ; 	retract(lock(Session)),
	broadcast(rolog_result(Session, Alias, Expr, Result))
    ).

r_close(Session) :-
    <- 'RS.close'(Session).

:- dynamic lock/1.

idle(Session) :-
    \+ lock(Session),
    !.

idle(Session) :-
    thread_wait(\+ lock(Session), [wait_preds([-(lock/1)])]).

test :-
    r_init(s1),
    r_init(s2),

    r_eval(s1, sum(abs(sin(1:10000000))), Res1),
    format("s1-sync: Res1 = ~w~n", [Res1]),
    r_eval(s2, sum(abs(sin(1:10000000))), Res2),
    format("s2-sync: Res2 = ~w~n", [Res2]),

    listen(rolog_result(S, A, E, R), format("Session ~w, Alias ~w, Expr ~w, Result ~w~n", [S, A, E, R])),

    time(r_submit(s1, a, sum(abs(sin(1:10000000))))),
    time(r_submit(s1, b, sum(abs(sin(1:1000000))))),
    time(r_submit(s2, c, sum(abs(sin(1:30000000))))),
    time(r_submit(s2, d, sum(abs(sin(1:3000000))))),

    r_eval(s1, sum(abs(sin(1:100))), Res3),
    format("s1-sync: Res3 = ~w~n", [Res3]),
    r_eval(s2, sum(abs(sin(1:100))), Res4),
    format("s2-sync: Res4 = ~w~n", [Res4]),

    r_close(s1),
    r_close(s2).
