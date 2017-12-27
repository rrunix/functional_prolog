:- op( 500, fx, if).
:- op( 600, xfy, then).
:- op( 600, xfx, else).

% Execute the expresion Exp of the clauses in File.
exec(Exp,File):-
   consult(File),
   eval(Exp, R),
   write(Exp),
   write('='),
   write(R).

% Convert functional file to prolog
term_expansion(DEF is Body , Clause) :-
	DEF =.. Terms,
        append(Terms, [__OUT], NewTerms),
        NewDef =.. NewTerms,
        Clause = :-(NewDef, eval(Body, __OUT)).


% Evaluate primitive-datatypes.
eval(Exp, Val) :- number(Exp), !, Val is Exp.

% If structure,
eval(if C then X else Y, Val) :-  !, (eval(C, 0) -> eval(Y, Val); eval(X, Val)).

% List-like structure
eval([], []) :- !.
eval([X|Xs], [Y|Ys]) :-
   eval(X, Y),
   eval(Xs, Ys), !.

% Evaluate arithmetical operations
eval(Exp, Val) :-
   Exp =.. [Func | Pargs],
   arithmetic_op(Func),
   !,
   eval(Pargs, Args),
   NewExp =.. [Func | Args],
   Val is NewExp.

% Evaluate comparisons
eval(Exp, Val) :-
   Exp =.. [Func | Pargs],
   comparator_op(Func),
   !,
   eval(Pargs, Args),
   func_op(Func, Args, Val).

% Evaluate any other function. It also works with standar-ones as long
% as the ouput is the last argument.
eval(Exp, Val) :-
   Exp =.. [Func | Pargs],
   current_predicate(Func, _),
   !,
   eval(Pargs, Args),
   append(Args, [Val], NewArgs),
   apply(Func, NewArgs).

% Not matching function found, raising exception.
eval(Exp, _):-
   throw(["Unrecognized Expresion", Exp]).

% Operator X is arithmetic
arithmetic_op(X) :-
   X = +; X = -; X = *; X = /; X = mod.

% Operator X is comparator
comparator_op(X) :-
   X = >; X = <; X = >=; X = <=; X = =:=; X = =\= .

% X is the result of applyng Operator Op to arguments Args. 1 means
% true, whereas 0 false.
func_op(Op, Args, X) :- (apply(Op, Args) -> X is 1; X is 0), !.
