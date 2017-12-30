:- use_module(library(write)).
:- op( 500, fx, if).
:- op( 600, xfy, then).
:- op( 600, xfx, else).

% exec(Exp, File): Execute the expresion Exp of the clauses in File.
exec(Exp,File):-
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
       def(T),
       read_func_lang_stream(S)
   ; true).

% def(X): Load the functional predicate X into prolog.
def(Def is Body) :-
   def(Def, Body), !.

def(X) :-
   write(["Warning: Ignoring malformed predicate; Functional Predicates are defined 'Def is Body'", X]).

% def(X): Load the functional predicate Def is Body into prolog.
def(Def, Body) :-
   expand_term(Def is Body, Clause),
   assertz(Clause).

% expand_term(X, Clause). Clause is the result of translating X from our
% functional language.
expand_term(DEF is Body , Clause) :-
	DEF =.. Terms,
        append(Terms, [__OUT], NewTerms),
        NewDef =.. NewTerms,
        Clause = :-(NewDef, eval(Body, __OUT)).

% eval(Exp, Val): Val is the result of evaluating the expresion Exp.
%
% Evaluate primitive-datatypes.
eval(Exp, Val) :- number(Exp), !, Val is Exp.

% If structure, works with both fail or 0 bool.
eval(if C then X else Y, Val) :-  !, ((eval(C, R), R =\= 0) -> eval(X, Val); eval(Y, Val)) .

% List-like structure,
% Note: this predicate rely on an extern predicate to perform the
% evaluation to increase the performance, but would be possible to
% achieve using a recursive call over eval.
eval([X|Xs], [Y|Ys]) :-
   eval_list([X|Xs], [Y|Ys]), !.

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
   exists_predicate(Func, _),
   !,
   eval(Pargs, Args),
   append(Args, [Val], NewArgs),
   apply_call(Func, NewArgs).

% Not matching function found, raising exception.
eval(Exp, _):-
   throw([unrecognized_expression, Exp]).

% eval_list(X, Y): Y is the resulting list of evaluating the
% individual values of X.
eval_list([], []).
eval_list([X|Xs], [Y|Ys]) :-
   eval(X, Y),
   eval_list(Xs, Ys).

% exists_predicate(Pred, Args): Predicate Pred with arity
% length(Args) is defined.
exists_predicate(Pred, Args) :-
   length(Args, X),
   current_predicate(Pred/X).

% arithmetic_op(X): X is an arithmetical operator
arithmetic_op(X) :-
   X = +,   !;
   X = -,   !;
   X = *,   !;
   X = /,   !;
   X = mod, !.

% comparator_op(X): X is a comparator operator.
comparator_op(X) :-
   X = >,   !;
   X = <,   !;
   X = >=,  !;
   X = <=,  !;
   X = =:=, !;
   X = =\=, !.

% func_op(Op, Args, X) : X is the boolean result (0 if fail, 1 if
% succed) of evaluating the expresion Func(Args).
func_op(Op, Args, X) :- (apply_call(Op, Args) -> X is 1; X is 0).

% apply_call(Func, Args): Call predicate Func(Args).
apply_call(Func, Args) :-
   Expr =.. [Func | Args],
   call(Expr).














































