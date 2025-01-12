:- module(test_rolog, [test_rolog/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).

% Load the library from our pack that needs to be tested
:- use_module(library(rolog)).

test_rolog :-
    run_tests([rolog]).

:- begin_tests(rolog).

test(basic) :-
    r_eval(2 + 2, Res),
    assertion(Res =@= 4).

:- end_tests(rolog).
