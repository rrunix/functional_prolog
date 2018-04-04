:- op( 500, fx, if).
:- op( 600, xfy, then).
:- op( 600, xfx, else).
:- dynamic def/2.

% exec(Exp, File): Execute the expresion Exp using the functional
% definitions in the file File. All previous functional definitions
% are removed to avoid dependency clashing.
exec(Exp,File):-
   retractall(def(_, _)),
   read_func_lang(File),
   eval(Exp, R),
   format("~w = ~w\n", [Exp, R]).

% read_func_lang(File): Load into prolog the functional predicates in
% the file File.
read_func_lang(File) :-
   open(File, read, S),
   read_func_lang_stream(S).

% read_func_lang_stream(S): Load into prolog the funcional predicates in
% the stream S.
read_func_lang_stream(S):-
   catch(read(S, T),
		 error(Err, Context),
		 format("WARNING: Ignoring malformed predicate ~w. ~w\n", [Err, Context])),
   (T \== end_of_file ->
		(check_and_insert_func_predicate(T), read_func_lang_stream(S));
		true).

% check_and_insert_func_predicate(P) :- Check and insert the functional predicate P.
check_and_insert_func_predicate(P) :- var(P).
check_and_insert_func_predicate(P) :- assert_functional_predicate(P).


% assert_functional_predicate(X): Assert the functional predicate X.
assert_functional_predicate(Def is Body):-
   assert(def(Def, Body)), !.

assert_functional_predicate(X) :-
   format("Invalid functional predicate. ~w.\n", [X]).

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

% Evaluate free variable
eval(Exp, Val) :-
   var(Exp),
   !,
   format("Cannot evaluate free variables; replacing by 0\n"),
   Val is 0.

% Evaluate primitive-datatypes.
eval(Exp, Val) :- number(Exp), !, Val is Exp.

% If structure, false is represented as 0, true as a number distinct
% from 0.
eval(if C then X else Y, Val) :-  \+ number(C), !, eval(C, R), eval(if R then X else Y, Val).
eval(if 1 then X else _, Val):- !, eval(X, Val).
eval(if 0 then _ else Y, Val):- !, eval(Y, Val).
eval(if X then _ else _, _):-
	format("Boolean must be either 1 or 0, not ~d\n", [X]),
	abort.

% Evaluate arithmetical operations
eval(Exp, Val) :-
   Exp =.. [Func | Args],
   arithmetic_op(Func),
   !,
   eval_list(Args, Pargs),
   NewExp =.. [Func | Pargs],
   Val is NewExp.

% Evaluate comparisons
eval(Exp, Val) :-
   Exp =.. [Func | Args],
   boolean_op(Func),
   !,
   eval_list(Args, Pargs),
   NewExp =.. [Func | Pargs],
   bool_op(NewExp, Val).

% Evaluate definitions.
eval(Exp, Val) :-
   Exp =.. [Func | Args],
   eval_list(Args, PArgs),
   EvalExp =.. [Func | PArgs],
   def(EvalExp, NewExp),
   !,
   eval(NewExp, Val).
   % asserta(def(EvalExp, Val)). Uncomment to enable memoization

% Not matching function found, raising exception.
eval(Exp, _):-
   format("Expresion ~w not recognized\n", [Exp]),
   abort.

% Eval list-like structure
eval_list([], []) :- !.
eval_list([Exp|Exps], [Val|Vals]) :-
   eval(Exp, Val),
   eval_list(Exps, Vals), !.

% func_op(Exp, X) : X is the boolean result (0 if fail, 1 if
% succed) of evaluating the expresiQon Exp.
bool_op(Exp, X) :- (call(Exp) -> X is 1; X is 0).

% arithmetic_op(X): X is an arithmetic operator
arithmetic_op(+).
arithmetic_op(-).
arithmetic_op(*).
arithmetic_op(/).
arithmetic_op(mod).

% boolean_op(X): X is a boolean operator
boolean_op(>).
boolean_op(<).
boolean_op(>=).
boolean_op(=<).
boolean_op(=:=).
boolean_op(=\=).
