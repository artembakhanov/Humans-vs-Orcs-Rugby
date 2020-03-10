:- dynamic min/3.
:- dynamic player/1.
:- dynamic orc/1.
:- dynamic touchdown/1.
:- dynamic max_axis/2. 
:- dynamic visited/4. % used for BFS
:- dynamic hid/1. % used for BFS

hid(0).

input(Name) :-
    read_file_to_terms(Name, Terms, []),
    retractall(player(_)),
    retractall(orc(_)),
    retractall(touchdown(_)),
    retractall(max_axis(_, _)),
    maplist(assert, Terms).

% the current history of movings
min(9999, [], []).

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

% print the map and a solution on it.
print_column(N, _) :-
    N1 is N - 1,
    max_axis(N1, _),
    nl,
    !.
print_column(N, M) :-
    min(_, S, _),
    ((player(p(N, M)), write("p"));
    (orc(p(N, M)), write("o")); 
    (touchdown(p(N, M)), write("t")); 
    write(".")),
    ((member(p(N, M), S) -> write("* ")); write("  ")),
    N1 is N + 1, print_column(N1, M).

print_columns(-1).
print_columns(N) :-
    print_column(0, N),
    N1 is N - 1,
    print_columns(N1).

print_table :-
    max_axis(_, M),
    print_columns(M),
    !.
%########################################################

%#######################################################
% Backtracking Search
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
    (player(Ball1) -> NewPlayer = true;  NewPlayer = false).%👍😎😎😎😎😎😎😎 (works on Windows 9, do not try on Munir's MACBook)

% is valid move
valid(p(X, Y)) :-
    \+ orc(p(X, Y)),
    max_axis(MaxX, MaxY),
    between(0, MaxX, X),
    between(0, MaxY, Y).

backtracking_solve(Ball, H, T, _) :-
    touchdown(Ball),
    valid(Ball),
    min(T1, _,_),
    T < T1,
    retractall(min(_,_,_)),
    assertz(min(T, H, [])).

backtracking_solve(Ball, H, T, Pass) :-
    history_id(_),
    valid(Ball),
    min(T1, _, _),
    T < T1,
    move(Move, Mname),
    update(Ball, Move, Ball1, NewPlayer, H, Pass, Pass1),
    \+ member(Ball1, H),
    increment_time(T, NewPlayer, T2),
    backtracking_solve(Ball1, [Mname, Ball1|H], T2, Pass1).
%#######################################################

%#######################################################
% Random Search Implementation
select_random_move(Move, Name, Pass) :-
    (
        Pass = true, Moves = [up, down, right, left, e, ne, n, se, sw, s, w, nw];
        Pass = false, Moves = [up, down, right, left]
    ),
    random_member(Move, Moves),
    move(Move, Name),
    !.

random_search(Ball, H, T, _) :-
    T is 100;
    touchdown(Ball),
    valid(Ball),
    retractall(min(_,_,_)),
    assertz(min(T, H, [])).

random_search(Ball, H, T, Pass) :-
    T < 100,
    valid(Ball),
    select_random_move(Move, Mname, Pass),
    update(Ball, Move, Ball1, NewPlayer, H, Pass, Pass1),
    increment_time(T, NewPlayer, T2),
    random_search(Ball1, [Mname, Ball1|H], T2, Pass1).

random_move(Move, Mname) :- 
    Moves = [up, down, right, left, e, ne, n, se, sw, s, w, nw],
    random_permutation(Moves, RandomMoves),
    member(Move, RandomMoves),
    move(Move, Mname).
%#######################################################

imp_backtracking_solve(Ball, H, T, _) :-
    touchdown(Ball),
    min(T1, _,_),
    T < T1,
    retractall(min(_,_,_)),
    assertz(min(T, H, [])).

imp_backtracking_solve(Ball, H, T, Pass) :-
    valid(Ball),
    min(T1, _, _),
    T < T1,
    random_move(Move, Mname),
    update(Ball, Move, Ball1, NewPlayer, H, Pass, Pass1),
    \+ member(Ball1, H),
    increment_time(T, NewPlayer, T2),
    imp_backtracking_solve(Ball1, [Mname, Ball1|H], T2, Pass1).

%########################################################
% BFS algrorithm.
% This is the best algorithm here. 

% Check if the cell was not visited.
% Not visited means that the cell was not literally visited; 
% or the cell was visited but in the history that has longer distance or used ball pass.
not_visited(Ball, T, Type) :-
    (\+ visited(_, Ball, _, _); 
    (visited(Time, Ball, _, _), 
    T < Time; visited(Time, Ball, Type1, _), Type = true, Type1 = false)),
    !.

% This predicate writes a cell to the knowledge base.
% This predicate is called everytime when the system uses a cell
% that was not used before.
write_visited(Ball, RT2, Type, Hid) :-
    (Type = true; 
    ((visited(RT1, Ball, Type1, _), (Type = false) -> (Type1 = false; RT2 < RT1)); \+visited(_, Ball, _, _))),
    retractall(visited(_, Ball, _, _)),
    assertz(visited(RT2, Ball, Type, Hid)).

% This predicates return a unique if for a  BFS 'history'.
% It is used for histories identifications.
history_id(A1) :-
    hid(A),
    A1 is A + 1,
    retractall(hid(_)),
    assertz(hid(A1)).

% This function creates new history for a given one.
% It performs one move and return new history.
% Note that I do not return a history with ball with the position 
% that was already visited before.
bfs_new_history(history(_, Ball, Pass, T, RT, H), NHistory) :-
    valid(Ball),
    move(Move, Mname),
    update(Ball, Move, p(X1, Y1), NewPlayer, H, Pass, Pass1),
    Ball1 = p(X1, Y1),
    Ball = p(X, Y),
    valid(Ball1),
    increment_time(T, NewPlayer, T2),
    ((Mname = move -> RT2 is T2);
    (Mname = pass -> 
        Dx is X1 - X,
        Dy is Y1 - Y,
        abs(Dx, Dxa),
        abs(Dy, Dya),
        RT2 is RT + Dxa + Dya - 1
        )),
    not_visited(Ball1, RT2, Pass1),
    history_id(Hid1),
    write_visited(Ball1, RT2, Pass1, Hid1),
    NHistory = history(Hid1, Ball1, Pass1, T2, RT2, [Mname, Ball1|H]).

% The base case of recursive BFS.
bfs_solve([history(_, Ball, _, T, _, H)|Tail]) :-
    touchdown(Ball),
    valid(Ball),
    min(X, _, _),
    (T < X ->
    retractall(min(_,_,_)),
    assertz(min(T, H, [])); true),
    bfs_solve(Tail).

bfs_solve([]).

%########################################################
% BFS implementation. l
% In the beginning you should [history(p(0, 0), true, 0, [])] as the first argument.
% It gets all the possible histories for the head history. It pops it (like in BFS) and push new histories.
bfs_solve([History|Tail]) :-
    findall(NewHistory, bfs_new_history(History, NewHistory), NewHistories),
    append(Tail, NewHistories, Merged),
    %write(History), write(" "), write(NewHistories), nl,
    bfs_solve(Merged).
%########################################################

%########################################################
% the predicates below run particular methods 
run_backtracking_search :-
    backtracking_solve(p(0, 0), [], 0, true),
    %findall(_, backtracking_solve(p(0, 0), [], 0, true), _), % uncomment this to find the best path
    !.

run_random_search(T) :-
    T > 0,
    (
    T1 is T - 1,
    run_random_search(T1);
    random_search(p(0, 0), [], 0, true)),
    !.
    
run_bfs :-
    retractall(visited(_, _, _, _)),
    retractall(hid(_)),
    assertz(hid(0)),
    bfs_solve([history(0, p(0, 0), true, 0, 0, [])]).

run_imp_backtracking_search :-
    findall(_, imp_backtracking_solve(p(0, 0), [], 0, true), _), % uncomment this to find the best path
    !.
%########################################################

%########################################################
% Printing a given solution 
print_solution1([]).
print_solution1([p(X, Y), H1 | T]) :-
    (H1 = pass -> Mes = "P "; H1 = move -> Mes = ""),
    format('~s~d ~d~n', [Mes, X, Y]),
    print_solution1(T).

print_solution(9999, _, ExecutionTime) :-
    write("No solution. "), nl, write(ExecutionTime), write(" msec"), nl, !.

print_solution(Size, Solution, ExecutionTime) :-
    reverse(Solution1, Solution),
    write(Size), nl,
    print_solution1(Solution1),
    write(ExecutionTime), write(" msec"), nl.
%########################################################

start(N) :- start(N, 1).
start(N, Test, PrintTable) :- start(N, Test), PrintTable -> print_table.
start(N, Test) :- 
    retractall(min(_, _, _)),
    retractall(hid(_)),
    assertz(hid(0)),
    assertz(min(9999, [], [])),
    nth0(N, [run_backtracking_search, run_random_search(100000), run_bfs], Algorithm),
    format(atom(TestName), "tests/input~d.pro", [Test]),
    input(TestName),
    statistics(walltime, [_ | [_]]),
    (Algorithm; true),
    statistics(walltime, [_ | [ExecutionTime]]),
    min(X, Y, _),
    print_solution(X, Y, ExecutionTime),
    !.
