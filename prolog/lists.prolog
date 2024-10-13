
% P01 (*) Find the last element of a list.
my_last(X, [_|Y]) :- my_last(X, Y).
my_last(X, [X]).

my_lastt(Last, [X|Xs]) :- my_lastt_(Xs, X, Last).
my_lastt_([], Last, Last).
my_lastt_([X|Xs], _, Last) :- my_lastt_(Xs, X, Last).

% P02 (*) Find the last but one element of a list.
last_but1(LastBut1, [LastBut1,_]).
last_but1(LastBut1, [_ |Xs]) :- last_but1(LastBut1, Xs).

% P03 (*) Find the K'th element of a list.
element_at(E, [E|_], 1).
element_at(E, [_|Es], N) :- element_at(E, Es, M), N is M+1.

% P04 (*) Find the number of elements of a list.
my_length([], 0).
my_length([_|L], N) :- my_length(L, M), N is M+1.

% P05 (*) Reverse a list.
my_reverse(L, R) :-
  my_reverse_(L, [], R).
my_reverse_([], Acc, Acc).
my_reverse_([H|T], Acc, R) :- my_reverse_(T, [H|Acc], R).

% P06 (*) Find out whether a list is a palindrome.
is_palindrome(L) :- my_reverse(L, L).

% P07 (**) Flatten a nested list structure.
my_flatten([], []).
my_flatten([H|T], F) :-
  is_list(H),
  my_flatten(T, F1),
  my_flatten(H, H1),
  append(H1, F1, F).
my_flatten([H|T], [H|F]) :-
  my_flatten(T,F).

% P08 (**) Eliminate consecutive duplicates of list elements.
compress([], []).
compress([H,H|T], T1) :- compress([H|T], T1).
compress([H|T], [H|T1]) :- compress(T, T1).

% P09 (**) Pack consecutive duplicates of list elements into sublists.
% TODO: could be made 2-way?
pack(L, R) :- pack_(L, [], R).
pack_([], Acc, Packed) :- my_reverse(Acc, Packed).
pack_([H|T], [[H|Hs]|Acc], Packed) :- pack_(T, [[H,H|Hs]|Acc], Packed).
pack_([H|T], Acc, Packed) :- pack_(T, [[H]|Acc], Packed).

% P10 (*) Run-length encoding of a list.
encode(L, R) :- encode_(L, [], R).
encode_([], Acc, E) :- my_reverse(Acc, E).
encode_([H|T], [[H,N]|Acc], E) :- encode_(T, [[H,M]|Acc], E), M is N+1.
encode_([H|T], Acc, E) :- encode_(T, [[H,1]|Acc], E).

% vim: ft=prolog
