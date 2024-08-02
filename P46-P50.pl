% Truth tables for logical expressions.
% Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 
% (for logical equivalence) which succeed or fail according to the result of their 
% respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed. 

% Note that A and B can be Prolog goals (not only the constants true and fail).
% A logical expression in two variables can then be written in prefix notation,
% as in the following example: and(or(A,B),nand(A,B)).
and(X, Y) :- X, Y.
nand(X, Y) :- \+and(X, Y).
or(X, Y) :- X; Y.
nor(X, Y) :- \+or(X, Y).
xor(X, Y) :- \+X, Y; X, \+Y.
impl(X, Y) :- Y; \+X, \+Y.
equ(X, Y) :- X, Y; \+X, \+Y.
not(X) :- \+X.


% Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

% Example:
% ?- table(A,B,and(A,or(A,B))).
% true true true
% true fail true
% fail true fail
% fail fail fail
tf_gen(0, []).
tf_gen(N, [true | R]) :-
    N > 0,
    Next is N - 1,
    tf_gen(Next, R).
tf_gen(N, [false | R]) :-
    N > 0,
    Next is N - 1,
    tf_gen(Next, R).

print_row(A, B, Expr) :-
    print(A),
    write(' '),
    print(B),
    write(' '),
    ( Expr -> print(true) ; print(false) ),
    write('\n').

table(A, B, Expr) :-
    tf_gen(2, [A, B]),
    print_row(A, B, Expr), 
    fail. % to iterate all combinations

% Continue problem P46 by defining and/2, or/2, etc as being operators. This 
% allows to write the logical expression in the more natural way, as in the 
% example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.
% Example:
% ?- table(A,B, A and (A or not B)).
% true true true
% true fail true
% fail true fail
% fail fail fail

init() :- op(98, xfy, and), op(100, xfy, or), op(90, fx, not), op(99, xfy, equ), op(99, xfy, impl).

% Truth tables for logical expressions (3).
% Generalize problem P47 in such a way that the logical expression may contain any
% number of logical variables. Define table/2 in a way that table(List,Expr) prints
% the truth table for the expression Expr, which contains the logical variables enumerated in List.
% Example:
% ?- gtable([A,B,C], A and (B or C) equ A and B or A and C).
% true true true true
% true true fail true
% true fail true true
% true fail fail true
% fail true true true
% fail true fail true
% fail fail true true
% fail fail fail true

gprintList([]).
gprintList([X | Xs]) :-
    ( X -> write('T ') ; write('F ') ),
    gprintList(Xs).

gprint_row(List, Expr) :-
    gprintList(List),
    ( Expr -> write('T') ; write('F') ),
    write('\n').

gtable(VarList, Expr) :-
    length(VarList, Len),
    tf_gen(Len, VarList),
    gprint_row(VarList, Expr), 
    fail. % to iterate all combinations


% An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
% n = 1: C(1) = ['0','1'].
% n = 2: C(2) = ['00','01','11','10'].
% n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].

% Find out the construction rules and write a predicate with the following specification:

% % gray(N,C) :- C is the N-bit Gray code

% Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?
prepend(_, [], []).
prepend(What, [X | Xs], [Y | Rest]) :- 
    string_concat(What, X, Y),
    prepend(What, Xs, Rest).


gray(1, ['0', '1']).
gray(N, C) :- 
    N > 1,
    Next is N - 1,
    gray(Next, LessBitGray),
    prepend('0', LessBitGray, ZeroPart),
    prepend('1', LessBitGray, OnePartRev),
    reverse(OnePartRev, OnePart),
    append(ZeroPart, OnePart, C).
    
% Huffman code.
% First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes!

% We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. Example: 
% [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) terms, 
% where C is the Huffman code word for the symbol S. In our example, the result could be Hs = 
% [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be 
% performed by the predicate huffman/2 defined as follows:

% % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
freqCount(L, R) :- freqCount(L, [], R).
freqCount([], A, A).
freqCount([C | Cs], Acc, R) :-
    insertCount(C, Acc, NewAcc),
    freqCount(Cs, NewAcc, R).

insertCount(C, [], [fr(C, 1)]).
insertCount(C, [fr(C, X) | Xs], [fr(C, Y) | Xs]) :- Y is X + 1.
insertCount(C, [fr(D, X) | Xs], [fr(D, X) | Rest]) :- 
    C \== D, 
    insertCount(C, Xs, Rest).    

pair([], []).
pair([fr(C, X) | Xs], [X-fr(C, X) | Rs]) :- pair(Xs, Rs).

unpair([], []).
unpair([ X-fr(C, X) | Xs], [fr(C, X) | Rs]) :- unpair(Xs, Rs).

put(Value, _, [], [Value]).
put(Value, Key, [fr(C1, X1) | Xs], [Value, fr(C1, X1) | Xs]) :-
    Key < X1.
put(Value, Key, [fr(C1, X1) | Xs], [fr(C1, X1) | Rs]) :-
    Key >= X1,
    put(Value, Key, Xs, Rs).
put(Value, Key, [node(X1, Left, Right) | Xs], [Value, node(X1, Left, Right) | Xs]) :-
    Key =< X1.
put(Value, Key, [node(X1, Left, Right) | Xs], [node(X1, Left, Right) | Rs]) :-
    Key > X1,
    put(Value, Key, Xs, Rs).

buildTree([X], X).
buildTree([fr(C1, X1), fr(C2, X2) | Xs], R) :-
    XRoot is X1 + X2,
    put(node(XRoot, fr(C1, X1), fr(C2, X2)), XRoot, Xs, Sorted),
    buildTree(Sorted, R).
buildTree([node(XRoot, Left, Right), fr(C2, X2) | Xs], R) :-
    NewRoot is XRoot + X2,
    put(node(NewRoot, node(XRoot, Left, Right), fr(C2, X2)), NewRoot, Xs, Sorted),
    buildTree(Sorted, R).
buildTree([fr(C2, X2), node(XRoot, Left, Right) | Xs], R) :-
    NewRoot is XRoot + X2,
    put(node(NewRoot, fr(C2, X2),  node(XRoot, Left, Right)), NewRoot, Xs, Sorted),
    buildTree(Sorted, R).
buildTree([node(X1, Left1, Right1), node(X2, Left2, Right2) | Xs], R) :-
    NewRoot is X1 + X2,
    put(node(NewRoot, node(X1, Left1, Right1), node(X2, Left2, Right2)), NewRoot, Xs, Sorted),
    buildTree(Sorted, R).

getFreqs(Text, Res) :-
    string_chars(Text, Chars),
    freqCount(Chars, Freqs),
    pair(Freqs, FreqPairs),
    keysort(FreqPairs, SortedPairs),
    unpair(SortedPairs, Res).

build(Text, Res) :-
    getFreqs(Text, Freqs),
    buildTree(Freqs, Res).

appendEvery(_, [], []).
appendEvery(Char, [[C, X] | Xs], [[C, Y]| Res]) :-
    string_concat(Char, X, Y),
    appendEvery(Char, Xs, Res).


getTable(node(_, fr(C1, _), fr(C2, _)), [[C1, '0'], [C2, '1']]). 
getTable(node(_, fr(C1, _), node(A, B, C)), R) :-
    getTable(node(A, B, C), Table),
    appendEvery('1', Table, NewTable),
    append([[C1, '0']], NewTable, R).
getTable(node(_, node(A, B, C), fr(C1, _)), R) :-
    getTable(node(A, B, C), Table),
    appendEvery('0', Table, NewTable),
    append([[C1, '1']], NewTable, R).
getTable(node(_, node(A, B, C), node(D, E, F)), R) :-
    getTable(node(A, B, C), LeftTable),
    getTable(node(D, E, F), RightTable),
    appendEvery('0', LeftTable, ZeroTable),
    appendEvery('1', RightTable, OneTable),
    append(ZeroTable, OneTable, R).
    
