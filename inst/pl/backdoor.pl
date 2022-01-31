% subset(?Subset, +Set)
%
% Tests if +Subset is subset of Set. Generates -Subsets of Set.
% Both Subset and Set are ordered.
subset([], []).
subset([H | Subset], [H | Set]) :-
    subset(Subset, Set).
subset(Subset, [_ | Set]) :-
    subset(Subset, Set).

% proper_subset(?Subset, +Set)
proper_subset(Subset, Set) :-
    subset(Subset, Set),
    length(Subset, M),
    length(Set, N),
    M < N.

% descendant(?X, ?Y)
% 
% transitive closure of arrow/2
descendant(X, Y) :-
    descendant(X, Y, [X]).

descendant(X, X, _Visited) :-
    node(X).

descendant(X, Y, Visited) :-
    arrow(X, Z),
    maplist(dif(Z), Visited),
    descendant(Z, Y, [Z | Visited]).

% descendant(+X, -Descendants)
% 
% all descendants of X
descendants(X, Descendants) :-
    findall(Y, descendant(X, Y), Descendants).

% candidates(+X, +Y, -Candidates)
% 
% candidates for adjustment: all nodes minus descendants of X and Y
candidates(X, Y, Candidates) :-
    findall(N, node(N), Nodes),
    descendants(X, DX),
    descendants(Y, DY),
    append(DX, DY, Descendants),
    subtract(Nodes, Descendants, Candidates).

% arc(?X, ?Y)
%
% connections (both directions)
arc(X, Y) :-
    arrow(X, Y) ; arrow(Y, X).

% path(+X, +Y, -Path)
%
% transitive closure of arc/2
path(X, Y, Path) :-
    path(X, Y, [X], Path).

path(X, X, Visited, Path) :-
    node(X),
    reverse(Visited, Path).

path(X, Y, Visited, Path) :-
    arc(X, Z),
    maplist(dif(Z), Visited),
    path(Z, Y, [Z | Visited], Path).

% backdoor(+X, +Y, -Path)
%
% paths that start with an arrow in the wrong direction
backdoor(X, Y, Path) :-
    arrow(Z, X),
    path(Z, Y, [Z, X], Path).

% is_intercept(-V, +Path)
% 
% true if V intercepts Path
is_intercept(V, Path) :-
    append([_, [_Prev, V, _Next], _], Path).

% has_intercept(+S, +Path)
% 
% once true if a member of S intercepts Path
has_intercept(S, Path) :-
    once((member(V, S), is_intercept(V, Path))).

% collider(-C, +Path)
% 
% true if C is a collider on Path
collider(C, Path) :-
    append([_, [Prev, C, Next], _], Path),
    arrow(Prev, C),
    arrow(Next, C).

noncollider(V, Path) :-
    is_intercept(V, Path),
    not(collider(V, Path)).

% unblocked(+X, +Y, -Path)
%
% true if Path is unblocked
unblocked(X, Y, Path) :-
    backdoor(X, Y, Path),
    not(collider(_, Path)).

% Backdoor test, criterion G1 (Greenland et al., 1999)
%
% Every unblocked backdoor path is intercepted by a variable in S.
g1(X, Y, S) :-
    candidates(X, Y, Nodes),
    subset(S, Nodes),
    findall(P, unblocked(X, Y, P), Paths),
    maplist(has_intercept(S), Paths).

% Criterion G2*
%
% If every collider on a backdoor path is either in S or has a descendant in S,
% then S must also have a noncollider on that path.
g2star(X, Y, S) :-
    candidates(X, Y, Nodes),
    subset(S, Nodes),
    findall(B, backdoor(X, Y, B), Backdoors),
    maplist(g2star(S), Backdoors).

overlaps(Set, Other) :-
    intersection(Set, Other, [_ | _]).

g2star(S, Backdoor) :-
    (   findall(C, collider(C, Backdoor), Colliders),
        maplist(descendants, Colliders, Descendants),
        maplist(overlaps(S), Descendants)
    ->  once((member(NC, S), noncollider(NC, Backdoor)))
    ;   true).

% Backdoor test, sufficiency
sufficient(X, Y, S) :-
    g1(X, Y, S),
    g2star(X, Y, S).

% minimal(+X, +Y, -Path)
% 
% Minimal sufficient set of covariates
minimal(X, Y, M) :-
    findall(S, sufficient(X, Y, S), Sufficient),
    member(M, Sufficient),
    findall(_, (member(Si, Sufficient), proper_subset(Si, M)), []).

% Figure 12
:- dynamic node/1.
node(a).
node(b).
node(c).
node(d).
node(e).
node(f).
node(u).

:- dynamic arrow/2.
arrow(a, d).
arrow(a, f).
arrow(b, d).
arrow(b, f).
arrow(c, d).
arrow(c, f).
arrow(e, d).
arrow(f, e).
arrow(u, a).
arrow(u, b).
arrow(u, c).

/** <examples>
?- minimal(e, d, S)
*/
