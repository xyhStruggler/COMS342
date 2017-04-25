%% Few more examples: Please try out the examples before reviewing the solutions
%%

%% 1. Done in class
%% Append List L1 to List L2. The result is a new list L, where
%% L1 is followed by L2.

%% 2.
%% Given a list L1 and L2, verify whether L1 is a prefix of L2.
%% prefix is builtin predicate in swipl, so use a new predicate
%% prefix1. Overwriting is not allowed.

%% 3.
%% Permutation
%% insert x in some specific position in a list
%% insert x in all positions of a list
%% insert x in all positions of all lists in a given list of lists
%% permute a list

%% **.
%% interpreter in logic - write the semantics for the language
%% containing variables



%%% Solutions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 1.
%% appending empty list to any list
append([], L, L).

%% appending non-empty list
append([X|Xs], L, [X|L1]) :-
    append(Xs, L, L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2. prefix

%% empty list is prefix of any list
prefix1([], _L).

%% first elements must match
prefix1([X|Xs], [X|Ys]) :- prefix1(Xs, Ys).

%% Some usage scenarios
%% ?- prefix([1, 2], [1, 2, 3, 4]).
%% true.

%% ?- prefix(X, [1, 2, 3, 4]).
%% X = [] ;
%% X = [1] ;
%% X = [1, 2] ;
%% X = [1, 2, 3] ;
%% X = [1, 2, 3, 4] ;
%% false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% position value [0, n-1], where n is the number of elements
%% in the list
%% insert(+X, ?L, +N, ?L1)

%% insert X to the empty list
insert(X, [], _, [X]).

%% insert in the 0-th position
%% guard L \= [] to ensure non-overlap with above rule
insert(X, L, 0, [X|L]) :- L \= [].

%% insert in n-th position if
%% first element is added to the head of the list
%% resulting in inserting in n-1th position
insert(X, [Y|Ys], N, [Y|L]) :-
    N > 0,
    N1 is N - 1,
    insert(X, Ys, N1, L).

%%%%% insertinallpos: insert an element in all position of a list
%% Get the length of the input list
%% call helper insertin to recursively use insert from above
insertinallpos(X, L, L1) :-
    length(L, Length),
    insertin(X, L, Length, L1).

%% inserting in -1 position will result in empty result
%% will be used as base case
insertin(_X, _L, -1, []).

%% recursively used insert to identify the result of
%% insertion at a particular position
insertin(X, L, N, [L1|L1s]) :-
    insert(X, L, N, L1),
    N1 is N - 1,
    insertin(X, L, N1, L1s).


%%insertinall: insert x in all the lists in all the positions
insertinall(_X, [], []).
insertinall(X, [L|Ls], Result) :-
    insertinallpos(X, L, L1),
    insertinall(X, Ls, Result1),
    append(L1, Result1, Result).

%% Now we are ready to do permutation
%% permuting empty list produces empty list
permute([], [[]]).

%% now with some elements in the list
permute([X|L], Result) :-
    permute(L, LResult), !, %% permute a smaller list, don't allow backtracking
    insertinall(X, LResult, Result). %% then just insert the first element everywhere


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simple arithmatic interpreter as per our grammar
%% Heap not used yet

%% Env: variable-value list, We will use the prolog list format [...]

%% semantics of number
eval(Expr, _Env, _Heap, Expr) :- number(Expr).

%% semantics of variable
eval(Expr, Env, _Heap, Val) :-
    atom(Expr), %% instead of symbol - as we have used in Racket
    findvalue(Expr, Env, Val).

%% need to comma-separate the terms
%% Also: for the list of variable assignments, let's use prolog list [...]
%% rather than Racket list (...)
eval((var, VarAssignLst, Expr), Env, Heap, Val) :-
    evalvarassign(VarAssignLst, Expr, Env, Heap, Val).

%% self-explanatory
eval((+, Expr1, Expr2), Env, Heap, Val) :-
    eval(Expr1, Env, Heap, Val1),
    eval(Expr2, Env, Heap, Val2),
    Val is Val1 + Val2.

findvalue(X, [(X, V)|_], V) :- !. %% don't backtrack further to look for more values
findvalue(X, [_|Rest], V) :-
    findvalue(X, Rest, V).

%% no variable assignments
evalvarassign([], Expr, Env, Heap, Val) :-
    eval(Expr, Env, Heap, Val).

evalvarassign([(X, E)|Rest], Expr, Env, Heap, Val) :-
    eval(E, Env, Heap, EVal),  %% evaluate the E
    evalvarassign(Rest, Expr, [(X, EVal)|Env], Heap, Val). %% construct new env

%% ?- eval((var, [(x, 2), (y, (+, x, 1))], (+, x, y)), [], [], V).
%% V = 5 ;
%% false.
