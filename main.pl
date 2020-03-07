:- dynamic min/3.
:- dynamic player/1.
:- dynamic orc/1.
:- dynamic touchdown/1.

input :-
    read_file_to_terms("tests/input2.pro", Terms, []),
    maplist(assert, Terms).

% ball pass directions
direction(n, p(0, 1)).
direction(ne, p(1, 1)).
direction(e, p(1, 0)).
direction(se, p(1, -1)).
direction(s, p(0, -1)).
direction(sw, p(-1, -1)).
direction(w, p(-1, 0)).
direction(nw, p(-1, 1)).

% possible moves (including ball passes)
move(X, pass) :- direction(X, _).
move(up, move).
move(right, move).
move(down, move).
move(left, move).

% updating the position
update(p(X, Y), up, p(X, Y1)) :- Y1 is Y + 1.
update(p(X, Y), right, p(X1, Y)) :- X1 is X + 1.
update(p(X, Y), down, p(X, Y1)) :- Y1 is Y - 1.
update(p(X, Y), left, p(X1, Y)) :- X1 is X - 1.

increment_time(T, true, T).
increment_time(T, false, T1) :-
    T1 is T + 1.

slice(A, 0, A).
slice([], N, []) :- N >= 0.
slice([_|T], N, A) :- N1 is N - 1, slice(T, N1, A).

pass(Ball, _, Ball1, NewPlayer, H) :-
    valid(Ball),
    slice(H, 2, T),
    \+ member(Ball, T),
    player(Ball),
    Ball1 = Ball,
    NewPlayer = false.

pass(p(X, Y), Move, Ball1, NewPlayer, H) :-
    direction(Move, p(Dx, Dy)),
    valid(p(X, Y)),
    slice(H, 2, T),
    \+ member(p(X, Y), T), \+ player(p(X, Y)),
    X1 is X + Dx, Y1 is Y + Dy,
    pass(p(X1, Y1), Move, Ball1, NewPlayer, T).

update(p(X, Y), Move, Ball1, NewPlayer, H, true, false) :-
    direction(Move, p(Dx, Dy)),
    X1 is X + Dx,
    Y1 is Y + Dy,
    pass(p(X1, Y1), Move, Ball1, NewPlayer, H).

update(Ball, Move, Ball1, NewPlayer, _, Pass, Pass) :-
    \+ direction(Move, _),
    update(Ball, Move, Ball1),
    find_player(Ball1, NewPlayer).

find_player(P, false) :-
    \+ player(P).

find_player(P, true) :-
    player(P).

% is valid move
valid(p(X, Y)) :-
    \+ orc(p(X, Y)),
    between(0, 4, X),
    between(0, 4, Y).

% the current history of movings
min(1000, [], []).

backtracking_solve(Ball, H, T, _) :-
    touchdown(Ball),
    min(T1, _,_),
    T < T1,
    retractall(min(_,_,_)),
    assertz(min(T, H, [])).

backtracking_solve(Ball, H, T, Pass) :-
    valid(Ball),
    min(T1, _, _),
    T < T1,
    move(Move, Mname),
    update(Ball, Move, Ball1, NewPlayer, H, Pass, Pass1),
    \+ member(Ball1, H),
    increment_time(T, NewPlayer, T2),
    backtracking_solve(Ball1, [Mname, Ball1|H], T2, Pass1).

select_random_move(Move, Name, Pass) :-
    (
        Pass = true, Moves = [up, down, right, left, e, ne, n, se, sw, s, w, nw];
        Pass = false, Moves = [up, down, right, left]
    ),
    random_member(Move, Moves),
    move(Move, Name),
    !.

random_search(Ball, H, T, _) :-
    (T is 100;
    touchdown(Ball)),
    %min(T1, _,_),
    %T < T1,
    retractall(min(_,_,_)),
    assertz(min(T, H, [])).

random_search(Ball, H, T, Pass) :-
    T < 100,
    valid(Ball),
    select_random_move(Move, Mname, Pass),
    update(Ball, Move, Ball1, NewPlayer, H, Pass, Pass1),
    increment_time(T, NewPlayer, T2),
    random_search(Ball1, [Mname, Ball1|H], T2, Pass1).

% the predicates below run particular methods 
run_backtracking_search :-
    findall(_, backtracking_solve(p(0, 0), [], 0, true), _),
    !.

run_random_search(T) :-
    T > 0,
    (
    T1 is T - 1,
    run_random_search(T1);
    random_search(p(0, 0), [], 0, true)),
    !.
print_solution1([]).
print_solution1([p(X, Y), H1 | T]) :-
    (H1 = pass -> Mes = "P "; H1 = move -> Mes = ""),
    format('~s~d ~d~n', [Mes, X, Y]),
    print_solution1(T).

print_solution(Size, Solution, ExecutionTime) :-
    reverse(Solution1, Solution),
    write(Size), nl,
    print_solution1(Solution1),
    write(ExecutionTime), write(" msec"), nl.

start(N) :- 
    nth0(N, [run_backtracking_search, run_random_search(10000)], Algorithm),
    input,
    statistics(walltime, [_ | [_]]),
    Algorithm,
    statistics(walltime, [_ | [ExecutionTime]]),
    min(X, Y, Z),
    print_solution(X, Y, ExecutionTime).
    % print(X),
    % print(Y),
    % print(Z).
start.