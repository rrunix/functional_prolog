% :- use_module(library(write)).
:- op( 500, fx, if).
:- op( 600, xfy, then).
:- op( 600, xfx, else).
:- dynamic def/2.

% exec(Exp, File): Execute the expresion Exp using the functional
% definitions in the file File. All previous functional definitions
% are removed to avoid dependency clashing.
exec(Exp,File):-
   retractall(def(_, _))
   read_func_lang(File),
   eval(Exp, R),
   write(Exp),
   write('='),
   write(R),
   write('\n').

% read_func_lang(File): Load into prolog the functional predicates in
% the file File.
read_func_lang(File) :-
   atom_concat(File, '.pl', FullFile),
   open(FullFile, read, S),
   read_func_lang_stream(S).

% read_func_lang_stream(S): Load into prolog the funcional predicates in
% the stream S.
read_func_lang_stream(S) :-
   read(S, T),
   (T \== end_of_file ->
       assert_functional_predicate(T),
       read_func_lang_stream(S)
   ; true).

% assert_functional_predicate(X): Assert the functional predicate X.
assert_functional_predicate(Def is Body):-
   assert(def(Def, Body)), !.

assert_functional_predicate(X) :-
   throw([invalid_functional_predicate, X]).

% eval(Exp, Val): Val is the result of evaluating the expresion Exp. The
% expression is an arithmetical operation, so the result is a number.
%
% Operators available available are: +, -, /, * and mod.
% Operands available are numbers, and the result of applying
% definitions, which are templates.
%
% Definitions are defined as def/2, concretely def(A, B). When an
% operand in eval matches A, then it is substituted by B.
%
% Evaluate primitive-datatypes.
eval(Exp, Val) :- number(Exp), !, Val is Exp.

% If structure, false is represented as 0, true as a number distinct
% from 0.
eval(if C then X else Y, Val) :-  !, ((eval(C, 0)) -> eval(Y, Val); eval(X, Val)) .

% Eval list-like structure
eval([], []) :- !.
eval([Exp|Exps], [Val|Vals]) :-
   eval(Exp, Val),
   eval(Exps, Vals), !.

% Evaluate arithmetical operations
eval(Exp, Val) :-
   Exp =.. [Func | Args],
   arithmetic_op(Func),
   !,
   eval(Args, Pargs),
   NewExp =.. [Func | Pargs],
   Val is NewExp.

% Evaluate comparisons
eval(Exp, Val) :-
   Exp =.. [Func | Args],
   comparator_op(Func),
   !,
   eval(Args, Pargs),
   NewExp =.. [Func | Pargs],
   bool_op(NewExp, Val).

% Evaluate definitions.
eval(Exp, Val) :-
   def(Exp, NewExp),
   !,
   eval(NewExp, Val).

% Not matching function found, raising exception.
eval(Exp, _):-
   throw([unrecognized_expression, Exp]).

% arithmetic_op(X): X is an arithmetical operator
arithmetic_op(X) :-
   X = +   ;
   X = -   ;
   X = *   ;
   X = /   ;
   X = mod .

% comparator_op(X): X is a comparator operator.
comparator_op(X) :-
   X = >   ;
   X = <   ;
   X = >=  ;
   X = <=  ;
   X = =:= ;
   X = =\= .

% func_op(Exp, X) : X is the boolean result (0 if fail, 1 if
% succed) of evaluating the expresion Exp.
bool_op(Exp, X) :- (call(Exp) -> X is 1; X is 0).
