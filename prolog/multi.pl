:- module(rs_rolog, [rs_init/1, rs_eval/3, rs_submit/3, rs_close/1]).

:- reexport(library(rolog)).
:- use_module(library(broadcast)).

rs_init(Session) :-
    <- library('RSclient'),
    Session <- 'RS.connect'().

rs_eval(Session, Expr, Result) :-
    idle(Session),
    Result <- 'RS.eval'(Session, Expr, wait=true).

rs_submit(Session, Alias, Expr) :-
    idle(Session),
    assert(lock(Session)),
    <- 'RS.eval'(Session, Expr, wait=false),
    thread_create(rs_collect(Session, Alias, Expr), _).

rs_collect(Session, Alias, Expr) :-
    Result <- 'RS.collect'(Session, timeout=0),
    (   Result = [] % check if this is "null"
     -> sleep(0.1),
        rs_collect(Session, Alias, Expr)
     ; 	retract(lock(Session)),
	broadcast(rs_result(Session, Alias, Expr, Result))
    ).

rs_close(Session) :-
    <- 'RS.close'(Session).

:- dynamic lock/1.

idle(Session) :-
    \+ lock(Session),
    !.

idle(Session) :-
    thread_wait(\+ lock(Session), [wait_preds([-(lock/1)])]).

test :-
    rs_init(s1),
    rs_init(s2),

    rs_eval(s1, sum(abs(sin(1:10000000))), Res1),
    format("s1-sync: Res1 = ~w~n", [Res1]),
    rs_eval(s2, sum(abs(sin(1:10000000))), Res2),
    format("s2-sync: Res2 = ~w~n", [Res2]),

    listen(rs_result(S, A, E, R), format("Session ~w, Alias ~w, Expr ~w, Result ~w~n", [S, A, E, R])),

    time(rs_submit(s1, a, sum(abs(sin(1:10000000))))),
    time(rs_submit(s1, b, sum(abs(sin(1:1000000))))),
    time(rs_submit(s2, c, sum(abs(sin(1:30000000))))),
    time(rs_submit(s2, d, sum(abs(sin(1:3000000))))),

    rs_eval(s1, sum(abs(sin(1:100))), Res3),
    format("s1-sync: Res3 = ~w~n", [Res3]),
    rs_eval(s2, sum(abs(sin(1:100))), Res4),
    format("s2-sync: Res4 = ~w~n", [Res4]),

    rs_close(s1),
    rs_close(s2).
