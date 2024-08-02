% https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

% 1
% ?- my_last(X,[a,b,c,d]).
% X = d
my_last(X, [X]).
my_last(R, [_ | Rs]) :- my_last(R, Rs).

% Find the last but one element of a list.
% ?- lbo([a,b,c,d], X).
% X = c
lbo([X , _ | []], X).
lbo([_ | Rs], Y) :- lbo(Rs, Y).

% The first element in the list is number 1.
% Example:
% ?- element_at(X,[a,b,c,d,e],3).
% X = c
element_at(Elem, List, N) :- element_at(Elem, List, N, 1).
element_at(H, [H | _], N, N). 
element_at(Elem, [_ | Rs], N, K) :- 
        K < N,
        NextK is K + 1,
        element_at(Elem, Rs, N, NextK).

% Find the number of elements of a list.
% ?- count(a, [a, b, c, a, a, a, c, v, b, a], X)
% ?- X = 5
count(X, [X | Rs], Y) :- 
    count(X, Rs, Z),
    Y is Z + 1.
count(Y, [X | Rs], R) :-
    count(Y, Rs, R),
    X \== Y.
count(X, [X], 1).
count(X, [Y], 0) :- X \== Y.

% Reverse a list
% ?- my_rev([a, b, c, d, a, f, g, b, c], X).
% ?- X = [c, b, g, f, a, d, c, b, a]
my_rev(X, Y) :- my_rev(X, [], Y).
my_rev([], Y, Y).
my_rev([H | Rs], A, Y) :-  my_rev(Rs, [H | A], Y).

% Find out whether a list is a palindrome.
% ?- pali([a, b, b, a]).
% ?- true.
% ?- pali([a, b, c, b, a]).
% ?- true.
pali([]).
pali([H | Rs]) :- my_rev([H | Rs], [H | Rs]).

% Flatten a nested list structure.
% Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
% Example:
% ?- my_flatten([a, [b, [c, d], e]], X).
% X = [a, b, c, d, e]
my_is_list([]).
my_is_list([_ | _]).

my_flatten(List, Res) :- my_flatten(List, [], Res).
my_flatten([], Acc, Acc).
my_flatten([List | Rest], Acc, Res) :- my_is_list(List), my_flatten(Rest, Acc, NewAcc), my_flatten(List, NewAcc, Res).
my_flatten([NonListH | Rest], Acc, [NonListH | Res]) :- \+my_is_list(NonListH), my_flatten(Rest, Acc, Res).


% If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

% Example:
% ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [a,b,c,a,d,e]
compress(X, R) :- compress(X, [], R2), my_rev(R2, R).
compress([], A, A).
compress([X | Xs], [], R) :- compress(Xs, [X], R).
compress([X | Xs], [X | As], R) :- compress(Xs, [X | As], R).
compress([X | Xs], [Y | As], R) :- X \== Y, compress(Xs, [X, Y |As], R).


% If a list contains repeated elements they should be placed in separate sublists.

% Example:
% ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
consume(_, [], [], []).
consume(X, [Y | Ys], [], [Y | Ys]) :- X \== Y.
consume(X, [X | Xs], [X | R], Rest) :- consume(X, Xs, R, Rest).

pack(L, R) :- pack(L, [], R).
pack([], A, A).
pack([X | Xs], Acc, [Consumed | R]) :- 
    consume(X, [X | Xs], Consumed, Rest), 
    pack(Rest, Acc, R).

% Run-length encoding of a list.
% Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
% Consecutive duplicates of elements are encoded as terms [N,E] where N is the number of duplicates of the element E.

% Example:
% ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],[1,b],[2,c],[2,a],[1,d],[4,e]]
len([], 0).
len([_ | Xs], R) :-
    len(Xs, K),
    R is K + 1.

encode([], []).
encode([X | Xs], [[Len, X]| R]) :- 
    consume(X, [X | Xs], Consumed, Rest),
    len(Consumed, Len),
    encode(Rest, R).

% Modify the result of problem P10 in such a way that if an element has no duplicates it is simply 
% copied into the result list. Only elements with duplicates are transferred as [N,E] terms.

% Example:
% ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],b,[2,c],[2,a],d,[4,e]]

encode_modified([], []).
encode_modified([X | Xs], [X | R]) :- 
    consume(X, [X | Xs], Consumed, Rest),
    len(Consumed, 1),
    encode_modified(Rest, R).
encode_modified([X | Xs], [[Len, X]| R]) :- 
    consume(X, [X | Xs], Consumed, Rest),
    len(Consumed, Len),
    Len \= 1,
    encode_modified(Rest, R).

% Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
explode(_, Target, Target, 0).
explode(X, [X | Target], Acc, N) :-
    N > 0,
    Next is N - 1,
    explode(X, Target, Acc, Next).

decode([], []).
decode([[Len, X]| Xs], R) :- decode(Xs, Acc), explode(X, R, Acc, Len).
decode([X | Xs], [X | R]) :- \+my_is_list(X), decode(Xs, R). 

% Implement the so-called run-length encoding data compression method directly. 
% I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, 
% but only count them. As in problem P11, simplify the result list by replacing the singleton terms [1,X] by X.

% Example:
% ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],b,[2,c],[2,a],d,[4,e]]
count_same(_, [], [], 0).
count_same(X, [Y | Ys], [Y | Ys], 0) :- X \== Y.
count_same(X, [X | Xs], R, N) :-
    count_same(X, Xs, R, Next),
    N is Next + 1.

encode_direct([], []).
encode_direct([X | Xs], [X | R]) :- 
    count_same(X, [X | Xs], NotXList, 1),
    encode_direct(NotXList, R).
encode_direct([X | Xs], [[Len, X]| R]) :- 
    count_same(X, [X | Xs], NotXList, Len),
    Len \= 1,
    encode_direct(NotXList, R).

% Duplicate the elements of a list.
% Example:
% ?- dupli([a,b,c,c,d],X).
% X = [a,a,b,b,c,c,c,c,d,d]
dupli([], []).
dupli([X | Xs], [X, X | Rs]) :- dupli(Xs, Rs).

% Example:
% ?- dupli([a,b,c],3,X).
% X = [a,a,a,b,b,b,c,c,c]

% What are the results of the goal:
% ?- dupli(X,3,Y).
insert_n(X, N, S, R) :- insert_n(X, N, 0, S, R).
insert_n(_, N, N, S, S).
insert_n(X, N, Curr, S, [X | R]) :- 
    Curr < N,
    Next is Curr + 1,
    insert_n(X, N, Next, S, R).

dupli([], _, []).
dupli(_, 0, []).
dupli([X | Xs], N, R) :- 
    insert_n(X, N, Z, R),
    dupli(Xs, N, Z).