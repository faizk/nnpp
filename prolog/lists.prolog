
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
