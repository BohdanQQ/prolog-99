% Check whether a given term represents a binary tree
% Write a predicate istree/1 which succeeds if and only if its argument is a Prolog term representing a binary tree.
% Example:
% ?- istree(t(a,t(b,nil,nil),nil)).
% Yes
% ?- istree(t(a,t(b,nil,nil))).
% No
istree(nil).
istree(t(L, R)) :- istree(L), istree(R).

% Construct completely balanced binary trees
% In a completely balanced binary tree, the following property holds for every node: 
% The number of nodes in its left subtree and the number of nodes in its right subtree are 
% almost equal, which means their difference is not greater than one.

% Write a predicate cbal_tree/2 to construct completely balanced binary trees for 
% a given number of nodes. The predicate should generate all solutions via backtracking. 
% Put the letter 'x' as information into all nodes of the tree.
% Example:
% ?- cbal_tree(4,T).
% T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
% T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
% etc......No

cbal_tree(0, nil).
cbal_tree(1, t(x, nil, nil)).
cbal_tree(X, t(x, L, R)) :-
    X > 1,
    TX is X - 1,
    Mod is TX mod 2,
    LX is ((TX - Mod) / 2) + Mod,
    RX is (TX - Mod) / 2,
    cbal_tree(LX, L),
    cbal_tree(RX, R).
cbal_tree(X, t(x, L, R)) :-
    X > 1,
    TX is X - 1,
    Mod is TX mod 2,
    LX is ((TX - Mod) / 2) + Mod,
    RX is (TX - Mod) / 2,
    RX \== LX,
    cbal_tree(RX, L),
    cbal_tree(LX, R).

% Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image 
% of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. 
% Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. 
% We are only interested in the structure, not in the contents of the nodes.

symmetric(nil).
symmetric(t(_, X, Y)) :- mirror(X, Y).

mirror(nil, nil).
mirror(t(_, L, R), t(_, L2, R2)) :-
    mirror(L, R2),
    mirror(R, L2).

% Binary search trees (dictionaries)
% Use the predicate add/3, developed in chapter 4 of the course, to write a predicate
%  to construct a binary search tree from a list of integer numbers.
% Example:
% ?- construct([3,2,5,7,1],T).
% T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))

addNode(X, t(Y, L, nil), t(Y, L, t(X, nil, nil))) :- X >= Y.
addNode(X, t(Y, L, R), t(Y, L, NewR)) :-  R \== nil, X >= Y, addNode(X, R, NewR).
addNode(X, t(Y, nil, R), t(Y, t(X, nil, nil), R)) :- X < Y.
addNode(X, t(Y, L, R), t(Y, NewL, R)) :- L \== nil, X < Y, addNode(X, L, NewL).

construct([], nil).
construct([X | Xs], T) :- construct(Xs, t(X, nil, nil), T).

construct([], A, A).
construct([X | Xs], Acc, Res) :-
    addNode(X, Acc, NewAcc),
    construct(Xs, NewAcc, Res).


% Then use this predicate to test the solution of the problem P56.
% Example:
% ?- test_symmetric([5,3,18,1,4,12,21]).
% Yes
% ?- test_symmetric([3,2,5,7,4]).
% No
test_symmetric(List) :-
    construct(List, Tree),
    symmetric(Tree).

% Generate-and-test paradigm
% Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes. Example:
% ?- sym_cbal_trees(5,Ts).
% Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]

% How many such trees are there with 57 nodes? Investigate about how many solutions there are for a given number of nodes? What if the number is even? Write an appropriate predicate.

sym_cbal_trees(N, T) :-
    cbal_tree(N, T),
    symmetric(T).

% Construct height-balanced binary trees
% In a height-balanced binary tree, the following property holds for every node: 
% The height of its left subtree and the height of its right subtree are almost equal, 
% which means their difference is not greater than one.

% Write a predicate hbal_tree/2 to construct height-balanced binary trees for a given height. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
% Example:
% ?- hbal_tree(3,T).
% T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
% T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
% etc......No

hbal_tree(0, nil).
hbal_tree(N, t(x, Left, Right)) :-
    N > 0,
    Next is N - 1,
    hbal_tree(Next, Left),
    hbal_tree(Next, Right).
hbal_tree(N, t(x, Left, Right)) :-
    N > 0,
    Next is N - 1,
    NextLess is Next - 1,
    hbal_tree(Next, Left),
    hbal_tree(NextLess, Right).
hbal_tree(N, t(x, Left, Right)) :-
    N > 0,
    Next is N - 1,
    NextLess is Next - 1,
    hbal_tree(NextLess, Left),
    hbal_tree(Next, Right).


% jiribenes window------
% shl([X | Xs], R) :- append(Xs, [X], R).

% rot(X, X).
% rot(X, Y) :- 
%     same_length(X, Y),
%     shl(X, XSh), rot(XSh, Y).

% rotation & same length (omni-directional)
% sl([], []).
% sl([_ | Xs], [_ | Ys]) :- sl(Xs, Ys).

% rot(X, Y) :- sl(X, Y), append(F, S, X), append(S, F, Y).
% ----------------------
countNodes(Tree, N) :- countNodes(Tree, 0, N).
countNodes(nil, Acc, Acc) :- !.
countNodes(t(_, Left, Right), Acc, Res) :- 
    NewAcc is Acc + 1,
    countNodes(Left, NewAcc, IAcc),
    countNodes(Right, IAcc, Res).


% Construct height-balanced binary trees with a given number of nodes
% Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?
% Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more difficult.
% Try to find a recursive statement and turn it into a predicate minNodes/2 defined as follwos:

% % minNodes(H,N) :- N is the minimum number of nodes in a height-balanced binary tree of height H.
% (integer,integer), (+,?)

minNodes(1, 1) :- !.
minNodes(2, 2) :- !.
minNodes(H, N) :-
    H > 2,
    HLess is H - 1,
    HLesser is HLess - 1,
    minNodes(HLess, Left),
    minNodes(HLesser, Right),
    N is Left + Right + 1.

% On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have?

% % maxHeight(N,H) :- H is the maximum height of a height-balanced binary tree with N nodes
% (integer,integer), (+,?)

% max height - using min nodes?
%  

% genRangeDown(0, 0) :- !.
% genRangeDown(N, Curr) :-
%     Next is Curr - 1,
%     genRangeDown(Next, Next).

genRangeDown(X, To, X) :- X >= To.
genRangeDown(From, To, Now) :-
    From > To,
    Next is From - 1,
    genRangeDown(Next, To, Now).


genRangeUp(X, To, X) :- X =< To.
genRangeUp(From, To, Now) :-
    From < To,
    Next is From + 1,
    genRangeUp(Next, To, Now).

maxHeight(1, 1).
maxHeight(2, 2).
maxHeight(3, 2).
maxHeight(N, H) :-
    N > 2,
    Next is N - 2,
    NextNext is Next - 1,
    maxHeight(Next, AH),
    maxHeight(NextNext, BH),
    (AH < BH -> H is BH + 1 ; H is AH + 1).

% Now, we can attack the main problem: construct all the height-balanced binary trees with a given nuber of nodes.

% % hbal_tree_nodes(N,T) :- T is a height-balanced binary tree with N nodes.

% SLOW - MIGHT BE DUE TO PREVIOUS PREDICATES
% INCORRECT (probably) - DUE TO PREVIOUS PREDICATES' CORRECTNESS
hbal_tree_nodes(N, T) :-
    maxHeight(N, MaxHeight),
    hbal_tree_nodes_impl(N, MaxHeight, T).

hbal_tree_nodes_impl(N, Height, T) :-
    minNodes(Height, MinNodes),
    MinNodes =< N,
    hbal_tree(Height, T),
    countNodes(T, Nodes),
    Nodes == N.
hbal_tree_nodes_impl(N, Height, T) :- 
    Height > 0,
    Next is Height - 1,
    hbal_tree_nodes_impl(N, Next, T).


% Find out how many height-balanced trees exist for N = 15.


% Count the leaves of a binary tree
% A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.

% count_leaves(T,N) :- the binary tree T has N leaves

% todo: can be improved to take advantage of an accumulator
count_leaves(nil, 0).
count_leaves(t(_, nil, nil), 1) :- !.
count_leaves(t(_, Left, Right), R) :-
    count_leaves(Left, LeftCount),
    count_leaves(Right, RightCount),
    R is RightCount + LeftCount.


% Collect the leaves of a binary tree in a list
% A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.

% leaves(T,S) :- S is the list of all leaves of the binary tree T
leaves(T, R) :- leaves(T, [], R).

leaves(nil, Acc, Acc).
% TODO: can be improved to not use append
leaves(t(X, nil, nil), Acc, R) :- append(Acc, [t(X, nil, nil)], R), !.
leaves(t(_, Left, Right), Acc, R) :-
    leaves(Left, Acc, NewAcc),
    leaves(Right, NewAcc, R).

% Collect the internal nodes of a binary tree in a list
% An internal node of a binary tree has either one or two non-empty successors. 
% Write a predicate internals/2 to collect them in a list.

% internals(T,S) :- S is the list of internal nodes of the binary tree T.
internals(T, S) :- internals(T, [], S).

internals(nil, Acc, Acc).
internals(t(_, nil, nil), Acc, Acc) :- !.
internals(t(Val, Left, Right), Acc, Res) :-
    internals(Left, [t(Val, nil, nil) | Acc], NewAcc),
    internals(Right, NewAcc, Res).

% Collect the nodes at a given level in a list
% A node of a binary tree is at level N if the path from the root 
% to the node has length N-1. The root node is at level 1. 
% Write a predicate atlevel/3 to collect all nodes at a given level in a list.

% atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L

atlevel(T, L, S) :- atlevel(T, L, [], S).
atlevel(_, 0, Acc, Acc).
atlevel(nil, _, Acc, Acc).
% correct the order here, no append
atlevel(t(A, B, C), 1, Acc, [t(A, B, C) | Acc]) :- !.
atlevel(t(_, L, R), N, Acc, Res) :-
    NextLevel is N - 1,
    atlevel(L, NextLevel, Acc, NewAcc),
    atlevel(R, NextLevel, NewAcc, Res).


% A complete binary tree with height H is defined as follows: The levels 1,2,3,...,H-1 
% contain the maximum number of nodes (i.e 2**(i-1) at the level i, note that we start counting
% the levels from 1 at the root). In level H, which may contain less than the maximum possible 
% number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree 
% traversal all internal nodes come first, the leaves come second, and empty successors 
% (the nil's which are not really nodes!) come last.

% Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

% We can assign an address number to each node in a complete binary tree by enumerating the nodes 
% in levelorder, starting at the root with number 1. In doing so, we realize that for every node X
% with address A the following property holds: The address of X's left and right successors are 2*A
% and 2*A+1, respectively, supposed the successors do exist. This fact can be used to elegantly 
% construct a complete binary tree structure. Write a predicate complete_binary_tree/2 with the following specification:

% % complete_binary_tree(N,T) :- T is a complete binary tree with N nodes. (+,?)
% [4, 2, 5, 1, 3, 6, 7]

% TODO
% genSeq(N, R) :- genSeq(1, N, R).
% genSeq(0, _, []).
% genSeq(Curr, N, R) :-
%     Curr > 0,
%     (Curr mod 2 = 1 -> Half is Curr / 2 ; Half is Curr  // 2),
%     ToAddLeft is Half,
%     ToAddRight is Curr + 1,


% only for full trees
getTreeA(From, To, nil) :- From > To, !.
getTreeA(To, To, t(nil, To, nil)) :- !.
getTreeA(From, To, t(L, Root, R)) :-
    X is (To - From + 2) / 2,
    Root is From + X - 1,
    LTop is Root - 1,
    RBot is Root + 1,
    getTreeA(From, LTop, L),
    getTreeA(RBot, To, R).

% test
% genRange(Curr, To, Curr) :- Curr =< To.
% genRange(Curr, To, X) :- Curr < To, Next is Curr + 1, genRange(Next, To, X).


% gen(1, []).
% gen(N, [[] | Xs]) :- N > 1, Next is N - 1, gen(Next, Xs).
% gen(N, [X | [Xs]]) :- 
%     N > 2,
%     NLess is N - 1,
%     genRange(2, NLess, R),
%     gen(R, X),
%     Snd is NLess - R,
%     gen(Snd, Xs).
% gen(N, [X]) :- N > 2, Next is N - 1, gen(Next, X).

% gen(X) :- gen_l(1, X).
% gen_l(N, X) :- gen(N, X).
% gen_l(N, X) :- Next is N + 1, gen_l(Next, X).

iso(nil, nil, nil).
iso(t(A, B, C), t(A, B, C), t(L, f, R)) :-
    iso_f(A, C, L),
    iso_f(C, A, R).
iso(t(A, B, C), t(X, B, Y), t(L, t, R)) :-
    A \= C,
    iso(A, Y, L),
    iso(C, X, R).    
iso(t(A, B, C), t(X, B, Y), t(L, f, R)) :-
    A \= C,
    iso(A, X, L),
    iso(C, Y, R).
iso_f(nil, nil, nil).
iso_f(t(A, _, C), _, t(L, f, L)) :-
    iso_f(A, C, L).

% P64

% nil represents the empty tree (as usual)
% t(X,Y,L,W,R) represents a (non-empty) binary tree with root W "positioned" at (X,Y), and subtrees L and R
% Write a predicate layout_binary_tree/2 with the following specification:
% layout_binary_tree(T,PT) :- PT is the "positioned" binary tree obtained from the binary tree T. (+,?)

% layout_binary_tree(t(t(t(nil, lla, nil), la, t(t(nil, lrla, nil), rla, nil)), a, t(nil, ra, nil)), T)
layout_binary_tree(T1, LT) :-
    layout_binary_tree(1, 0, T1, LT, _).

layout_binary_tree(_, LeftOffStart, nil, nil, LeftOffStart).
layout_binary_tree(Depth, LeftOffStart, t(L, Val, R), t(LayL, LeftOffTemp, Depth, Val, LayR), LeftOff) :-
    NextDepth is Depth + 1,
    layout_binary_tree(NextDepth, LeftOffStart, L, LayL, LeftOffLeft),
    LeftOffTemp is LeftOffLeft + 1,
    layout_binary_tree(NextDepth, LeftOffTemp, R, LayR, LeftOff).

% 65
% An alternative layout method is depicted in the illustration opposite. Find out the rules and write the 
% corresponding Prolog predicate. Hint: On a given level, the horizontal distance between neighboring nodes is constant.

% Use the same conventions as in problem P64 and test your predicate in an appropriate way.

neededSpace(nil, 0).
neededSpace(t(R, _ , L), Res) :-
    neededSpace(R, Rspace),
    neededSpace(L, Lspace),
    Mx is Rspace + Lspace,
    Res is Mx + 1.
    

layout2(T1, RT) :-
    neededSpace(T1, SpaceX),
    layout2i(1, 0, SpaceX, T1, RT).


layout2i(_, _, _, nil, nil).
layout2i(Depth, StartX, MaxX, t(L, Val, R), t(LayL, RootX, Depth, Val, LayR)) :-
    NextDepth is Depth + 1,
    RootX is StartX + ( MaxX - StartX ) // 2,
    LR is RootX - 1,
    layout2i(NextDepth, StartX, LR, L, LayL),
    MR is RootX + 1,
    layout2i(NextDepth, MR, MaxX, R, LayR).


% Somebody represents binary trees as strings of the following type (see example opposite):
% a(b(d,e),c(,f(g,)))

% a) Write a Prolog predicate which generates this string representation, if the tree is given as usual
% (as nil or t(X,L,R) term). Then write a predicate which does this inverse; i.e. given the string 
% representation, construct the tree in the usual form. Finally, combine the two predicates in a single 
% predicate tree_string/2 which can be used in both directions.

ttrepr(nil, '').
ttrepr(t(nil, V, nil), VStr) :- atom_string(V, VStr).
ttrepr(t(L, V, R), Text) :-
    atom_string(V, VStr),
    string_concat(VStr, '(', Head),
    ttrepr(L, LStr),
    string_concat(LStr, ',', BodyL),
    ttrepr(R, RStr),
    string_concat(BodyL, RStr, BodyR),
    string_concat(BodyR, ')', Body),
    string_concat(Head, Body, Text).
    
% TODO: how to deconstruct `a(b, ) ==.. X`

