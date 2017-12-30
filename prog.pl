    fib(N) is
       if (N<2) then N
           else fib(N-1)+fib(N-2).

    even(N) is ( N mod 2 =:= 0 ).

    odd(N) is ( N mod 2 =\= 0 ).

    case(A,B,X,Y,Z) is
       if A then ( if B then X else Y ) else Z.
