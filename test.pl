:- [lang].

% Primitive data-types (integer, decimal)
:- eval(1, 1).
:- eval(1.1, 1.1).

% Arithmetical operations
:- eval(1+1, 2).
:- eval(2*2, 4).
:- eval(1-1, 0).
:- eval(2/2, 1).
:- eval(2 mod 2, 0).

% Comparisons
:- eval(1 > 0, 1).
:- eval(1 < 2, 1).
:- eval(1 =:= 1, 1).
:- eval(2 =\= 1, 1).

:- eval(1 < 0, 0).
:- eval(1 > 2, 0).
:- eval(1 =:= 0, 0).
:- eval(1 =\= 1, 0).

% If construct
:- eval(if 1 then 2 else 3, 2).
:- eval(if 0 then 2 else 3, 3).

% Insert some definitions to check complex terms
def(increment(X),  X+1).
def(add(X, Y), X+Y).
def(add_inc(X, Y), add(increment(X), Y)).
def(fib(N), if (N<2) then N else fib(N-1)+fib(N-2)).
def(case(A,B,X,Y,Z),if A then ( if B then X else Y ) else Z).
def(even(N), (N mod 2 =:= 0)).
def(odd(N), (N mod 2 =\= 0)).

% Definitions
:- eval(increment(1), 2).
:- eval(increment(7), 8).
:- eval(add(1, 2), 3).
:- eval(add_inc(1, 2), 4).
:- eval(fib(1), 1).
:- eval(fib(2), 1).
:- eval(fib(7), 13).

% More complex stuff
:- eval(increment(increment(1)), 3).
:- eval(2 - add(1, increment(3)), -3).
:- eval(2 - (if(increment(2) < 1) then (increment(4)) else (add(3,4))), -5).
:- eval(if (if (2 < 1) then 0 else 1) then 2 else 3, 2).
:- eval(fib(5)-fib(3), 3).
:- eval(case(odd(fib(5)),even(fib(7)),11,10,0), 10).




