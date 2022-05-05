:- use_module(library(dcg/basics)).

% Translate R string to code points and invoke phrase/2
sentence(Tree, Sentence) :-
    string_codes(Sentence, Codes),
    phrase(s(Tree), Codes).

% Simple grammar with sentences, noun, verb and participle phrases
s(s(NP, VP)) --> np(NP, C), blank, vp(VP, C).
np(NP, C) --> pn(NP, C).
np(np(Det, N), C) --> det(Det, C), blank, n(N, C).
np(np(Det, N, PP), C) --> det(Det, C), blank, n(N, C), blank, pp(PP).
vp(vp(V, NP), C) --> v(V, C), blank, np(NP, _).
vp(vp(V, NP, PP), C) --> v(V, C), blank, np(NP, _), blank, pp(PP).
pp(pp(P, NP)) --> p(P), blank, np(NP, _).

% Determiners, personal nouns, nouns, verbs and participles
det(det(a), sg) --> `a`.
det(det(the), _) --> `the`.
pn(pn(john), sg) --> `john`.
n(n(man), sg) --> `man`.
n(n(men), pl) --> `men`.
n(n(telescope), sg) --> `telescope`.
v(v(sees), sg) --> `sees`.
v(v(see), pl) --> `see`.
p(p(with)) --> `with`.
