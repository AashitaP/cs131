%checks if list is unique and within certain range and length - not used

unique_list(N, List) :-
    length(List, N),
    fd_domain(List, 1, N), 
    fd_all_different(List),
    fd_labeling(List).


%transposes matrix - derived from old SWI-PROLOG implementation 
%https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


%checks the towers
count_tower(_, [], 0).
    
count_tower(THead, [H|R], Top) :- 
    THead #< H, 
    fd_labeling(H),
    count_tower(H, R, X),
    Top is X+1.


count_tower(THead, [H|R], Top) :-
    THead #> H, 
    fd_labeling(H),
    count_tower(THead, R, Top).
    
count([],[]).  
    
count([Head|Tail], [Tophead|Toprest]) :-
    count_tower(0, Head, Tophead),
    count(Tail, Toprest).


%rotates list of list
reverseT([], []).

reverseT([THead|TRest], [TRHead|TRRest]) :-
    reverse(THead, TRHead),
    reverseT(TRest, TRRest).

rotate_right(T, TRight) :-
    transpose(T, TTranspose),
    reverseT(TRight, TTranspose).

%checks tables lengths
check_lengths(_, []).

check_lengths(N, [Head|Rest]) :-
    length(Head, N),
    check_lengths(N, Rest).

%checks tables is within a range
check_domains(_, []).

check_domains(N, [Head|Rest]) :-
    fd_domain(Head, 1, N), 
    check_domains(N, Rest).

tower(N, T, C) :-
    C = counts(Top, Bottom, Left, Right),
    length(T,N),
    check_lengths(N, T),
    rotate_right(T, TDown),
    check_domains(N, T),
    maplist(fd_all_different, T),
    check_domains(N, TDown),
    maplist(fd_all_different, TDown),
    maplist(fd_labeling, T),
    count(T, Left),
    count(TDown, Bottom),
    rotate_right(TDown, TRight),
    count(TRight, RRight),
    rotate_right(TRight,TUp),
    count(TUp, RTop),
    reverse(Right, RRight),
    reverse(Top, RTop).

%fill the table with numbers between 1 and N

fill_numbers(N, L) :-
	findall(Num, between(1, N, Num), X), 
    permutation(X, L).

%count tower without FD
plain_count_tower(_, [], 0).
    
plain_count_tower(THead, [H|R], Top) :- 
    THead < H, 
    plain_count_tower(H, R, X),
    Top is X+1.

plain_count_tower(THead, [H|R], Top) :-
    THead > H, 
    plain_count_tower(THead, R, Top).
    
plain_count([],[]).  
    
plain_count([Head|Tail], [Tophead|Toprest]) :-
    plain_count_tower(0, Head, Tophead),
    plain_count(Tail, Toprest).


plain_tower(N,T,C) :-
    C = counts(Top, Bottom, Left, Right),
    length(T,N),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),
    check_lengths(N, T),
    maplist(fill_numbers(N), T),
    plain_count(T, Left),
    rotate_right(T, TDown),
    plain_count(TDown, Bottom),
    rotate_right(TDown, TRight),
    plain_count(TRight, RRight),
    rotate_right(TRight,TUp),
    plain_count(TUp, RTop),
    reverse(Right, RRight),
    reverse(Top, RTop),
    maplist(fill_numbers(N), TDown).

%speedup measurements - measures total CPU time
tower_time(X, 0) :-
    X is 0.

tower_time(TimeTaken, N) :-
    N > 0,
    statistics(cpu_time, [SinceStart1, _]),
	tower(4, T, counts([4, 3, 2, 1], [1, 2, 2, 2], [4, 3, 2, 1], [1, 2, 2, 2])),
    statistics(cpu_time, [SinceStart2, _]),
    TimeTaken1 is SinceStart2 - SinceStart1,
    N1 is N - 1,
    tower_time(TimeTaken2, N1), 
    TimeTaken is TimeTaken2 + TimeTaken1.

plain_tower_time(X, 0) :-
    X is 0.

plain_tower_time(TimeTaken, N) :-
    N > 0,
    statistics(cpu_time, [SinceStart1, _]),
	plain_tower(4, T, counts([4, 3, 2, 1], [1, 2, 2, 2], [4, 3, 2, 1], [1, 2, 2, 2])),
    statistics(cpu_time, [SinceStart2, _]),
    TimeTaken1 is SinceStart2 - SinceStart1,
    N1 is N - 1,
    plain_tower_time(TimeTaken2, N1), 
    TimeTaken is TimeTaken2 + TimeTaken1.

speedup(R) :-
	tower_time(T1, 10),
	plain_tower_time(T2, 10),
	R is T2 / T1.


%ambiguous tower
ambiguous(N,C,T1,T2) :-
    tower(N,T1,C),
    tower(N,T2,C),
    T1 \= T2.
