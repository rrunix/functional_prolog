# functional_prolog


The assignment consists in programming an interpreter of programs
written in a functional language, as described below.

The main predicate of the interpreter must be exec/2, as is also
described below. Please, use this name and its convention for its
arguments, otherwise checking your program might become difficult.

---------------------------------------------------------------- 
                      THE FUNCTIONAL LANGUAGE

* PROGRAM *
A program is a series of definitions of functions. The program is
run by giving an initial expresion (see below) to be evaluated. The
result of the execution is the (numeric) value obtained from the 
evaluation of the expression as per the definitions in the program.

* FUNCTION *
A definition of a function in a program has the following form:

   HEAD is EXPRESSION .

where HEAD is the name of a function (being defined) with a series
of variables separated by commas and in between parenthesis, which
correspond to the formal arguments of the function.

The name of the function identifies it. Only one definition per 
function is accepted. Observe that the definition must end in '.'.

* EXPRESSION *
Function definitions in programs (and the initial calls to a program
for executing it) consists in expressions which are evaluated and
return a numeric value.

An EXPRESSION can be:
    - a variable
    - a number
    - a call to a function (defined in the program) with arguments
      which each must be an EXPRESSION
    - a (predefined) operator aplied to arguments which each must be
      an EXPRESSION
    - an if-then-else, in the form:
         if EXPRESSION then EXPRESSION else EXPRESSION 

The predefined operators are all binary and have the same meaning as
in Prolog:
    - boolean: > >= =< < =:= =\=
    - arithmetic: + - * / mod

Variables and numbers are written as in Prolog. Calls to functions 
are written as calls to predicates in Prolog.

In an if-then-else, the if EXPRESSION (the condition) must be written
in between parenthesis if it is an operator expression or if-then-else;
the then and else expressions must be written in between parenthesis if
they are themselves also an if-then-else. Examples:

   if ( N<2 ) then N+2 else max(N+2,min(5,2*N))
   if A then ( if B then X else Y ) else Z
   if ( if ( N<2 ) then inverse(N) else N ) then N/2 else N*N
   if even(N) then 0 else 1

An operator expression which is the body of a function definition must
also be written in between parenthesis. Example:

even(N) is ( N mod 2 =:= 0 ).


* EVALUATION OF EXPRESSIONS *
Expressions are evaluated in the following form:
    - a boolean expression evaluates to 1 if true, to 0 if false
    - an arithmetic expression evaluates to the value obtained from the
      corresponding arithmetic operation 
    - an if-then-else evaluates to the value that corresponds to 
      evaluating either the then expression, if the if condition is true
      (1), or the else expression, if the if condition is false (0).
    - a call to a user function evaluates to the value that results from:
        - assigning to the formal arguments the values resulting from the
          evaluation of the expressions in the arguments of the call 
        - evaluating the expression which appears in the definition of
          the function with the corresponding values for the formal
          arguments

Predefined operators are reserved, that is, there is no need to (and 
should not be) defined, they are not definible by the user but defined by
default. They always evaluate as the corresponding boolean or arithmetic
operations. See EXECUTION for the concrete way of executing them.

---------------------------------------------------------------- 
                     READING IN PROGRAMS

The Prolog program must start by reading in the functional program from a
file and storing it in a suitable Prolog structure in order to be able to
execute it afterwards.

To do this, the function definitions read in from the file will be asserted
in Prolog and a dynamic predicate def/2 will be used for this purpose so
that:

   def(A,B) corresponds to function definition 'A is B'

Please use this predicate for assertions. You will also need to execute the
following operators/0 predicate BEFORE starting to read the file:

   operators:-
        op( 500, fx, if),
        op( 600, xfy, then),
        op( 600, xfx, else).

It is NOT NECESSARY to check that the functional program is syntactically
correct. It is assumed that it is correct (following the rules given above
in LANGUAGE). If it is not correct then either the definition will be
ignored (because it is simply not read) or the whole file reading fails or
it is the execution that fails later. You may choose any of these options.

Thus the Prolog program should only be able to execute functional programs
that are syntactically correct (except for the case of having more than one
definition of a function, see below).

---------------------------------------------------------------- 
                   EXECUTION OF PROGRAMS

The core of the Prolog program must be able to evaluate a functional
expression from the function definitions read in from the input file.
To do this, the following main predicate must be used:

   exec(Exp,File) writes on the screen the message
                    Exp = Val
                  where Val is the value resulting from evaluating
                  expression Exp in the program read from file File

When executing expressions, if an expression is not evaluable because
it is a (free) variable (free variables do not have values and therefore
cannot be evaluated) the Prolog program must write an error message and
take value 0 as the value for the expression and continue evaluating.

If the expression to be evaluated is a call to a non-existing function
(there is no definition and it is not a predefinied operator either) an
error message must be written and execution aborted (calling abort/0).

If evaluation of the condition in an if-then-else expression returns a
value other than 0 and 1, an error message must be written and execution
aborted.

If the expression to be evaluated is a call to a function that has more
than one definition, ONLY the first one (in the ordering in which they
have been read from the file) must be taken into account.

If the expression to be evaluated is a call to a predefined operator, it
will be evaluated by calling the equivalent Prolog operator.

---------------------------------------------------------------- 
                             EXAMPLE

File 'prog':

        fib(N) is
	       if (N<2) then N
               else fib(N-1)+fib(N-2).

        even(N) is ( N mod 2 =:= 0 ).

        odd(N) is ( N mod 2 =\= 0 ).

        case(A,B,X,Y,Z) is
	       if A then ( if B then X else Y ) else Z.

Prolog calls:

        ?- exec(fib(7),prog).
        fib(7) = 13

        yes
        ?- exec(fib(5)-fib(3), prog).
        fib(5)-fib(3) = 3

        yes
        ?- exec( case(odd(fib(5)),even(fib(7)),11,10,0), prog ).
        case(odd(fib(5)),even(fib(7)),11,10,0) = 10

        yes
        ?- 

---------------------------------------------------------------- 
