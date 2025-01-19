:- module(test_rolog, [test_rolog/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).

% Load the library from our pack that needs to be tested
:- use_module(library(rolog)).

test_rolog :-
    run_tests([basic, assignment, vector, indexing, empty]).

:- begin_tests(basic).

test(basic) :-
    r_eval(2 + 2, Res),
    assertion(Res =@= 4).

test(comparison1) :-
    r_eval(2 =< 3, Res),
    assertion(Res =@= true).

:- end_tests(basic).

:- begin_tests(assignment).

test(assignment1) :-
    r_call(v <- 1),
    r_eval(v, Res),
    assertion(Res =@= 1).

:- end_tests(assignment).

:- begin_tests(vector).

test(integer_vector) :-
    r_call(v <- #(1:5)),
    r_eval(v, Res),
    Res1 = '%%'(1, 2, 3, 4, 5),
    assertion(Res =@= Res1).

test(float_vector) :-
    r_call(v <- #(1.1:5.1)),
    r_eval(v, Res),
    Res1 = ##(1.1, 2.1, 3.1, 4.1, 5.1),
    assertion(Res =@= Res1).

test(char_vector) :-
    r_call(v <- #("foo", "bar")),
    r_eval(v, Res),
    Res1 = $$("foo", "bar"),
    assertion(Res =@= Res1).

test(boolean_vector) :-
    r_call(v <- #(true, false)),
    r_eval(v, Res),
    Res1 = '!!'(true, false),
    assertion(Res =@= Res1).

:- end_tests(vector).

:- begin_tests(indexing).

test(indexing1) :-
    r_call(v <- #(1:5)),
    r_eval(v[3], Res),
    assertion(Res =@= 3).

:- end_tests(indexing).

:- begin_tests(empty).

test(empty_brackets) :-
    r_eval({}, Res),
    assertion(Res =@= []).

:- end_tests(empty).
