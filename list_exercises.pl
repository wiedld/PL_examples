%! Example of exercises completed.


%!      list_sum(+List:list_of_numbers, -Sum:int) is det
%
%    Succeeds if Sum is the sum of the numbers in List
%
%    @arg List a list of numbers
%    @arg Sum their sum

list_sum(List, Sum) :-
  flatten(List, Out), % http://www.swi-prolog.org/pldoc/man?predicate=flatten%2f2
  do_sum(Out, Sum).

do_sum([], 0).
do_sum([X], X).

do_sum([H|T], Total) :-
    do_sum(T, A),
    Total is H + A,
    !.


%!   nth_in_list(+List:list, +Index:integer, -Value:term) is semidet
%
%   extract the 0 based Index element of List
%
%   @arg List    list to extract element from
%   @arg Index   index - 0 is first element
%   @arg Value   will be bound to the Index element on return

nth_in_list(List, Index, Value) :-
  \+ string(List),
  step_index(List, Index, Value, 0, 1),
  Index >= 0.

nth_in_list(List, Index, Value) :-
  \+ string(List),
  reverse(List, Reversed),
  step_index(Reversed, Index, Value, -1, -1),
  Index < 0.

nth_in_list(String, Index, Value) :-
  string(String),
  % atom_chars(String, CharList),
  % nth_in_list(CharList, Index, Value).
  atom_codes(String, CodeList),
  atom_code(Value, [Code]),
  nth_in_list(CodeList, Index, Code).

step_index([X|_], Index, X, Index, _).

step_index([_|List], Index, X, Step, Incr) :-
  NewStep is Step + Incr,
  step_index(List, Index, X, NewStep, Incr).







person(X) :-
  member(X, [alice,bob,charlene,derek,ethyl,faye,gilam,harvey]).
household(X) :-
  member(X, [phone,sheet,bed,chair,cup]).
industry(X) :-
  member(X, [factory,laboratory,machine_shop,foundry]).

mixed_stuff([ethyl, tacos, charlene, factory, phone, sheet, harvey]).

%!   stuff_category(?Stuff:list_of_stuff, ?Cat:list_of_categories) is nondet
%
%    @arg Stuff a list of atoms
%    @arg Cat a list of what category the element is in
%
stuff_category([], []).
stuff_category([H0|Stuff], [H1|Cat]) :-
  category(H0, H1),
  stuff_category(Stuff, Cat).

category(X, person) :- person(X).
category(X, household) :- household(X).
category(X, industry) :- industry(X).
category(_, none).




%!     filter(:Goal:callable, +List:list, -Filtered:list) is det
%
%    Succeeds if the last argument is a list of those elements
%    of the second argument for which the first argument
%    succeeds at least once
%
%    @arg Goal  the goal to test with an added arg
%    @arg List  the input list
%    @arg Filtered those elements of List for which Goal succeeds
%
filter(_, [], []).

% if you try a Variable (to autogen a list). Will fail.
filter(_, Var, _) :-
  var(Var),
  !, fail.

% passes filter
filter(Goal, [H0|List], [H1|Filtered]) :-
  call(Goal, H0),
  H0 = H1,
  !,
  filter(Goal, List, Filtered).

% fails filter
filter(Goal, [H0|List], Filtered) :-
  \+ call(Goal, H0),
  filter(Goal, List, Filtered).


goal(X) :- X > 14.

test0 :-
  filter(goal, [0,4,5], []).

test1 :-
  filter(goal, [15,17,19], [15,17,19]).

test2 :-
  filter(goal, [4,18,1,99], Ans),
  format('\nAns should exist: ~w', [Ans]).

test3 :-
  filter(goal, Ans, [18,99]),
  format('\nAns should not exist: ~w', [Ans]).
