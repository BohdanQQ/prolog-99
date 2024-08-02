% Drop every N'th element from a list.
% Example:
% ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
% X = [a,b,d,e,g,h,k]

drop(L, N, X) :- drop(L, N, 1, X).
drop([], _, _, []).
drop([_ | Xs], N, N, R) :- drop(Xs, N, 1, R).
drop([X | Xs], N, Curr, [X | R]) :-
    Curr < N,
    Next is Curr + 1,
    drop(Xs, N, Next, R).

% Split a list into two parts; the length of the first part is given.
% Do not use any predefined predicates.

% Example:
% ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
% L1 = [a,b,c]
% L2 = [d,e,f,g,h,i,k]

split(L, N, L1, L2) :- split(L, N, 0, L1, L2).
split(L, N, N, [], L).
split([], _, _, [], []).
split([X | Xs], N, C, [X | L1], L2) :-
    C < N,
    Next is C + 1,
    split(Xs, N, Next, L1, L2).

% Extract a slice from a list.
% Given two indices, I and K, the slice is the list containing the elements 
% between the I'th and K'th element of the original list (both limits included). 
% Start counting the elements with 1.

% Example:
% ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
% X = [c,d,e,f,g]
slice(L, Start, End, R) :- slice(L, Start, End, 1, R).
slice([], _, _, _, []).
slice([X | _], _, End, End, [X]).
slice([_ | Xs], Start, End, I, R) :-
    I < Start,
    Next is I + 1,
    slice(Xs, Start, End, Next, R).
slice([X | Xs], Start, End, I, [X | R]) :-
    I >= Start,
    I < End,
    Next is I + 1,
    slice(Xs, Start, End, Next, R).

% Rotate a list N places to the left.
% Examples:
% ?- rotate([a,b,c,d,e,f,g,h],3,X).
% X = [d,e,f,g,h,a,b,c]

% ?- rotate([a,b,c,d,e,f,g,h],-2,X).
% X = [g,h,a,b,c,d,e,f]
rotate(L, N, R) :- 
    N >= 0,
    rotate_left(L, N, [], R).
rotate(L, N, R) :-
    N < 0,
    rotate_right(L, N, R).

rv(O, R) :- rv(O, [], R). 
rv([], A, A).
rv([X | Xs], A, R) :- rv(Xs, [X | A], R).

app([], B, B).
app(A, [], A).
app([A | As], B, [A | X]) :- app(As, B, X).

rotate_left(L, 0, A, R) :- rv(A, B), app(L, B, R).
rotate_left([X | Xs], N, A, R) :-
    N > 0,
    Next is N - 1,
    rotate_left(Xs, Next, [X | A], R).

rotate_right(L, N, R) :- 
    length(L, Len),
    LShift is Len + N,
    rotate_left(L, LShift, [], R).

% Remove the K'th element from a list.
% Example:
% ?- remove_at(X,[a,b,c,d],2,R).
% X = b
% R = [a,c,d]

remove_at(X, [X | Xs], 1, Xs).
remove_at(XR, [X | Xs], N, [X | R]) :-
    N > 1,
    Next is N - 1,
    remove_at(XR, Xs, Next, R).

% Insert an element at a given position into a list.
% Example:
% ?- insert_at(alfa,[a,b,c,d],2,L).
% L = [a,alfa,b,c,d]

insert_at(X, List, 1, [X | List]).
insert_at(X, [Y | Rest], N, [Y | Res]) :-
    N > 1,
    Next is N - 1,
    insert_at(X, Rest, Next, Res).

% Create a list containing all integers within a given range.
% Example:
% ?- range(4,9,L).
% L = [4,5,6,7,8,9]

range(N, N, [N]).
range(Start, Stop, [Start | Res]) :-
    Start < Stop,
    NextStart is Start + 1,
    range(NextStart, Stop, Res).

% Extract a given number of randomly selected elements from a list.
% The selected items shall be put into a result list.
% Example:
% ?- rnd_select([a,b,c,d,e,f,g,h],3,L).
% L = [e,d,a]

rnd_select(_, 0, []).
rnd_select(Lst, N, [Rand | Res]) :-
    N > 0,
    Next is N - 1,
    random_member(Rand, Lst),
    rnd_select(Lst, Next, Res).

% Lotto: Draw N different random numbers from the set 1..M.
% The selected numbers shall be put into a result list.
% Example:
% ?- rnd_select2(6,49,L).
% L = [23,1,17,33,21,37]

rnd_select2(Count, Max, R) :-
    range(1, Max, Range),
    rnd_select2_impl(Count, Range, R).

rnd_select2_impl(0, _, []).
rnd_select2_impl(Count, Source, [Pick | R]) :-
    Count > 0,
    length(Source, SrcLen),
    range(1, SrcLen, IdxRange),
    rnd_select(IdxRange, 1, [RandIdx]),
    remove_at(Pick, Source, RandIdx, NewSrc),
    NextCount is Count - 1,
    rnd_select2_impl(NextCount, NewSrc, R).

% Generate a random permutation of the elements of a list.
% Example:
% ?- rnd_permu([a,b,c,d,e,f],L).
% L = [b,a,d,c,e,f]
rnd_permu(List, Perm) :- 
    length(List, Len), 
    rnd_select2_impl(Len, List, Perm).

% Extra: permutations
% ?- perm([a,b,c,d,e,f],L).
% L = [a,b,c,d,e,f] ;
% L = [a,b,c,d,f,e] ;
% ...
% http://kti.ms.mff.cuni.cz/~bartak/prolog/combinatorics.html
perm([],[]).
perm(List, [H | Perm]) :- delete(H, List, Rest), perm(Rest, Perm).
   
delete(X, [X | T], T).
delete(X, [H | T], [H | NT]) :- delete(X, T, NT).


% Combinations
% Example:
% ?- combination(3,[a,b,c,d,e,f],L, _). % _ is the rest of the elements (extension for future problems)
% L = [a,b,c] ;
% L = [a,b,d] ;
% L = [a,b,e] ;
% ...
combination(0, X, [], X).
combination(N, [X | Xs], [X | R], Rest) :- 
    N > 0,
    Next is N - 1,
    combination(Next, Xs, R, Rest).
combination(N, [X | Xs], Res, [X | Rest]) :-
    N > 0,
    combination(N, Xs, Res, Rest).

% Group the elements of a set into disjoint subsets.
% a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? 
% Write a predicate that generates all the possibilities via backtracking.

% Example:
% ?- group3([aldo,beat,carla,david,evi,flip,gary,hugo,ida],G1,G2,G3).
% G1 = [aldo,beat], G2 = [carla,david,evi], G3 = [flip,gary,hugo,ida]
% ...

group3(Ppl, G1, G2, G3) :- combination(2, Ppl, G1, R1), combination(3, R1, G2, R2), combination(4, R2, G3, _). 


% b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

% Example:
% ?- group([aldo,beat,carla,david,evi,flip,gary,hugo,ida],[2,2,5],Gs).
% Gs = [[aldo,beat],[carla,david],[evi,flip,gary,hugo,ida]]
% ...

sum([], 0).
sum([X | Xs], R) :-
    sum(Xs, I),
    R is X + I.

group(Ppl, Cnt, Groups) :-
    length(Ppl, Len),
    sum(Cnt, Len),
    groupI(Ppl, Cnt, Groups).

groupI(_, [], []).
groupI(Ppl, [X | Xs], [G | Gs]) :-
    combination(X, Ppl, G, Rest),
    groupI(Rest, Xs, Gs).

% Note that we do not want permutations of the group members; i.e. [[aldo,beat],...] is the same solution as [[beat,aldo],...].
% However, we make a difference between [[aldo,beat],[carla,david],...] and [[carla,david],[aldo,beat],...].


% Sorting a list of lists according to length of sublists
% a) We suppose that a list (InList) contains elements that are lists themselves. 
% The objective is to sort the elements of InList according to their length. E.g. short lists first, longer lists later, or vice versa.

% Example:
% ?- lsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
% L = [[o], [d, e], [d, e], [m, n], [a, b, c], [f, g, h], [i, j, k, l]]

% this implementation DOES NOT keep order of !same lenght lists! from [X | Xs] inside Rest
% find_min([X | Xs], R, Rest) :- 
%     length(X, Len),
%     find_mini(Xs, X, Len, R, Rest).
% find_mini([], CurrMini, _, CurrMini, []).
% find_mini([X | Xs], CurrMini, CLen, R, [CurGrMini | Rest]) :-
%     length(X, Len),
%     Len < CLen,
%     find_mini(Xs, X, Len, R, Rest).
% find_mini([X | Xs], CurrMini, CLen, R, [X | Rest]) :-
%     length(X, Len),
%     Len >= CLen,
%     find_mini(Xs, CurrMini, CLen, R, Rest).

find_min([X], X, []).
find_min([X | Xs], X, [Y | Rest]) :-
    find_min(Xs, Y, Rest),
    length(Y, YLen),
    length(X, XLen),
    XLen =< YLen.
find_min([X | Xs], Y, [X | Rest]) :-
    find_min(Xs, Y, Rest),
    length(Y, YLen),
    length(X, XLen),
    XLen > YLen.

lsort([], []).
lsort([X | Xs], [Min | Rest]) :-
    find_min([X | Xs], Min, TempRest),
    lsort(TempRest, Rest).

% b) Again, we suppose that a list (InList) contains elements that are lists themselves. 
% But this time the objective is to sort the elements of InList according to their length frequency; i.e. in the default, 
% where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

% Example:
% ?- lfsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
% L = [[i, j, k, l], [o], [a, b, c], [f, g, h], [d, e], [d, e], [m, n]]
% Note that in the above example, the first two lists in the result L have length 4 and 1, 
% both lengths appear just once. The third and forth list have length 3 which appears, 
% there are two list of this length. And finally, the last three lists have length 2. This is the most frequent length.

% does not group different-length + same-frequency by lengths (a, ab, abc, ad, dce)
% initLenPairs([], []).
% initLenPairs([X  | Xs], [[L, X] | R]) :-
%     length(X, L),
%     initLenPairs(Xs, R).

% freq(_, [], 0).
% freq(X, [[Y, _] | Xs], R) :-
%     X \== Y,
%     freq(X, Xs, R).
% freq(X, [[Y, _] | Xs], R) :-
%     X == Y,
%     freq(X, Xs, P),
%     R is P + 1.

% fillFreq(Pairs, Result) :- fillFreqI(Pairs, Pairs, Result).
% fillFreqI(_, [], []).
% fillFreqI(Pairs, [[Len, X] | Xs], [[Freq, X] | R]) :-
%     freq(Len, Pairs, Freq),
%     fillFreqI(Pairs, Xs, R).


% unwrapPair([], []).
% unwrapPair([[_, X] | Xs], [X | R]) :- unwrapPair(Xs, R).

% find_pmin([X], X, []).
% find_pmin([[Len, List] | Xs], [Len, List], [[LenY, ListY] | Rest]) :-
%     find_pmin(Xs, [LenY, ListY], Rest),
%     Len =< LenY.
% find_pmin([[Len, List] | Xs], [LenY, ListY], [[Len, List] | Rest]) :-
%     find_pmin(Xs, [LenY, ListY], Rest),
%     Len > LenY.

% psort([], []).
% psort([X | Xs], [Min | Rest]) :-
%     find_pmin([X | Xs], Min, TempRest),
%     psort(TempRest, Rest).

% lfsort([], []).
% lfsort(L, R) :-
%     initLenPairs(L, Triplets),
%     fillFreq(Triplets, Pairs),
%     psort(Pairs, SortedPairs),
%     unwrapPair(SortedPairs, R).

% makes even more sense since it is reusing the previous solution
% groups by length => more of the same lengt => longer group
% sort by lsort solves the problem, only unwrapping the one layer is required
getSameLen(_, [], [], []).
getSameLen(N, [X | Xs], [X | Res], Rest) :-
    length(X, N),
    getSameLen(N, Xs, Res, Rest).
getSameLen(N, [X | Xs], Res, [X | Rest]) :-
    length(X, M),
    M \== N,
    getSameLen(N, Xs, Res, Rest).

groupByLen(L, R) :- groupByLenI(L, R).
groupByLenI([], []).
groupByLenI([X | Xs], [SameLen | Res]) :-
    length(X, LenX),
    getSameLen(LenX, [X | Xs], SameLen, Others),
    groupByLenI(Others, Res).

unwrapOneLayer([], []).
unwrapOneLayer([ []| Xs], Res) :- unwrapOneLayer(Xs, Res).
unwrapOneLayer([ [X | X2s] | Xs], [X | Res]) :- unwrapOneLayer([X2s | Xs], Res).

lfsort([], []).
lfsort(L, R) :-
    groupByLen(L, Temp),
    lsort(Temp, AlmostR),
    unwrapOneLayer(AlmostR, R).
