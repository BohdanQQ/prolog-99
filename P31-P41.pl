% Example:
% ?- is_prime(7).
% Yes

% very slow (see for example is_prime(134099447))
is_prime(2).
is_prime(X) :-
    X  > 2,
    Smaller is X - 1,
    is_prime_check(X, 2, Smaller).

% div_rem(X, Div, X) :- X < Div.
% div_rem(X, Div, R) :- 
%     X >= Div,
%     NextX is X - Div,
%     div_rem(NextX, Div, R).
div_rem(X, Div, R) :- R is X mod Div.

is_prime_check(_, Curr, Upper) :-
    Upper < Curr.
is_prime_check(X, Upper, Upper) :- 
    div_rem(X, Upper, Z),
    Z \== 0.
is_prime_check(X, Curr, Upper) :- 
    Curr < Upper, 
    div_rem(X, Curr, Z),
    Z \== 0,
    Next is Curr + 1,
    is_prime_check(X, Next, Upper).
is_prime_check(X, Curr, Upper) :- 
    Curr < Upper,
    NextCurr is Curr + 1,
    div_rem(X, NextCurr, Upper).

% Determine the greatest common divisor of two positive integer numbers.
% Use Euclid's algorithm.
% Example:
% ?- gcd(36, 63, G).
% G = 9
% Define gcd as an arithmetic function; so you can use it like this:
% ?- G is gcd(36,63).
% G = 9

m_gcd(0, X, X).
m_gcd(X, Y, R) :- 
    X > Y,
    m_gcd(Y, X, R).
m_gcd(X, Y, R) :-
    X \== 0,
    Y \== 0,
    X =< Y,
    Next is Y - X,
    m_gcd(Next, X, R).

% Determine whether two positive integer numbers are coprime.
% Two numbers are coprime if their greatest common divisor equals 1.
% Example:
% ?- coprime(35, 64).
% Yes

coprime(X, Y) :- m_gcd(X, Y, 1).

% Calculate Euler's totient function phi(m).
% Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
% Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

% ?- Phi is totient_phi(10).
% Phi = 4

phi(0, 0).
phi(1, 1).
phi(2, 1).
phi(X, R) :- phi(X, 1, R).
phi(X, Y, 1) :- Y is X - 1.
phi(X, Curr, R) :-
    X > Curr + 1,
    Next is Curr + 1,
    coprime(X, Curr),
    phi(X, Next, Rx),
    R is Rx + 1.
phi(X, Curr, R) :-
    X > Curr + 1,
    Next is Curr + 1,
    \+coprime(X, Curr),
    phi(X, Next, R).

% Determine the prime factors of a given positive integer.
% Construct a flat list containing the prime factors in ascending order.
% Example:
% ?- prime_factors(315, L).
% L = [3,3,5,7]
div_with_rem(X, Div, Rem, Res) :-
    div_rem(X, Div, Rem),
    Y is X - Rem,
    Res is Y / Div.


prime_factors(X, L) :- prime_factors(X, 2, L).
prime_factors(X, X, [X]).
prime_factors(1, _, []).
prime_factors(X, CurrDiv, [CurrDiv | L]) :-
    X > 1,
    CurrDiv < X,
    div_with_rem(X, CurrDiv, 0, R),
    prime_factors(R, CurrDiv, L).
prime_factors(X, CurrDiv, L) :-
    X > 1,
    CurrDiv < X,
    \+div_with_rem(X, CurrDiv, 0, _),
    NextDiv is CurrDiv + 1,
    prime_factors(X, NextDiv, L).

% Construct a list containing the prime factors and their multiplicity.
% Example:
% ?- prime_factors_mult(315, L).
% L = [[3,2],[5,1],[7,1]]
groupSame([], []).
groupSame([X], [[X, 1]]).
groupSame([X | Xs], [[X, R] | Rs]) :-
    groupSame(Xs, [[X, N] | Rs]),
    R is N + 1.
groupSame([X | Xs], [[X, 1], [Y, C] | Rs]) :-
    groupSame(Xs, [[Y, C] | Rs]),
    X \== Y.


prime_factors_mult(X, L) :-
    prime_factors(X, Flat),
    groupSame(Flat, L).

% Calculate Euler's totient function phi(m) (improved).
% See problem P34 for the definition of Euler's totient function. 
% If the list of the prime factors of a number m is known in the form of 
% problem P36 then the function phi(m) can be efficiently calculated 
% as follows: Let [[p1,m1],[p2,m2],[p3,m3],...] be the list of prime factors (and their multiplicities) of a given number m.
%  Then phi(m) can be calculated with the following formula:
% phi(m) = (p1 - 1) * p1**(m1 - 1) * (p2 - 1) * p2**(m2 - 1) * (p3 - 1) * p3**(m3 - 1) * ...

calcPhiF([], 1).
calcPhiF([[Factor, Exponent] | OtherFactors], R) :-
    calcPhiF(OtherFactors, Rest),
    FMinOne is Factor - 1,
    EMinOne is Exponent - 1,
    FtoEMinOne is Factor ** EMinOne,
    R is FMinOne * FtoEMinOne * Rest.


phimproved(X, R) :-
    prime_factors_mult(X, Factors),
    calcPhiF(Factors, R).

% Compare the two methods of calculating Euler's totient function.
% Use the solutions of problems P34 and P37 to compare the algorithms. 
% Take the number of logical inferences as a measure for efficiency. Try to calculate phi(10090) as an example.

% A list of prime numbers.
% Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

prime_range(Upper, Upper, []) :-
    \+is_prime(Upper).
prime_range(Upper, Upper, [Upper]) :-
    is_prime(Upper).
prime_range(Lower, Upper, R) :-
    Lower < Upper,
    \+is_prime(Lower),
    Next is Lower + 1,
    prime_range(Next, Upper, R).
prime_range(Lower, Upper, [Lower | R]) :- 
    Lower < Upper,
    is_prime(Lower),
    Next is Lower + 1,
    prime_range(Next, Upper, R).


% Goldbach's conjecture.
% Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. 
% Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct 
% in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with 
% our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
% Example:
% ?- goldbach(28, L).
% L = [5,23]

combination(0, X, [], X).
combination(N, [X | Xs], [X | R], Rest) :- 
    N > 0,
    Next is N - 1,
    combination(Next, Xs, R, Rest).
combination(N, [X | Xs], Res, [X | Rest]) :-
    N > 0,
    combination(N, Xs, Res, Rest).

goldbach(X, [A, B]) :-
    0 is X mod 2,
    Upper is X - 3,
    prime_range(2, Upper, Range),
    combination(2, Range, [A, B], _),
    X is A + B.
goldbach(X, [A, A]) :-
    0 is X mod 2,
    Upper is X / 2,
    prime_range(2, Upper, Range),
    combination(1, Range, [A], _),
    X is A + A.

% A list of Goldbach compositions.
% Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
% Example:
% ?- goldbach_list(9,20).
% 10 = 3 + 7
% 12 = 5 + 7
% 14 = 3 + 11
% 16 = 3 + 13
% 18 = 5 + 13
% 20 = 3 + 17

print_if_goldbach(X) :-
    goldbach(X, [A, B]),
    print(X),
    print( = ),
    print(A),
    print( + ),
    print(B),
    print(\n), !. % how do you print newline!
print_if_goldbach(_).

goldbach_list(Lower, Upper) :-
    Lower =< Upper,
    print_if_goldbach(Lower),
    Next is Lower + 1,
    goldbach_list(Next, Upper).