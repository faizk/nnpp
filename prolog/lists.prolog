
% P01 (*) Find the last element of a list. 
my_last(X, [_|Y]) :- my_last(X, Y).
my_last(X, [X]).

my_lastt(Last, [X|Xs]) :- my_lastt_(Xs, X, Last).
my_lastt_([], Last, Last).
my_lastt_([X|Xs], _, Last) :- my_lastt_(Xs, X, Last).

% P02 (*) Find the last but one element of a list.
last_but1(LastBut1, [LastBut1,_]).
last_but1(LastBut1, [_ |Xs]) :- last_but1(LastBut1, Xs).
