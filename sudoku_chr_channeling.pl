:- module(sudoku_chr_ovp, [solve/1, solveall/0]).
:- use_module(library(chr)).
:- chr_option(debug, off). % on - off
:- chr_option(optimize, full). % full - off
:- chr_option(check_guard_bindings, off). % on - off

:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type row == natural.
:- chr_type col == natural.
:- chr_type value == natural.
:- chr_type pos ---> row-col.
:- chr_type val --->  [value | list(value)].
:- chr_type rv ---> value-value.

:- chr_constraint cell(+pos, +val).
:- chr_constraint rvc(+rv, +list(natural)).
:- chr_constraint search(+natural). 
:- chr_constraint propagate, cleanup.
:- chr_constraint single(+row, +col, +value), remove(+row, +col, +value).

:- consult('sudex_toledo.pl').

show(Puzzle_name) :-
    puzzles(P, Puzzle_name),
    show(P).
show([]).
show([R1, R2, R3 | Rows]) :-
    portray(R1), portray(R2), portray(R3),
    show_(Rows).   
show_([]).
show_(Rows) :-
    write('------+-------+------'), nl,
    show(Rows).
portray(Elems) :-
    write_elems(Elems), nl.
write_elem(Elem) :-
    nonvar(Elem),
    write(Elem).
write_elem(_Elem) :-
    write(' ').
write_elems([]).
write_elems([E1, E2, E3 | Elems]) :-
    write_elem(E1), write(' '), write_elem(E2), write(' '), write_elem(E3),
    write_elems_(Elems).
write_elems_([]).
write_elems_(Elems) :-
    write(' | '),
    write_elems(Elems).

show_solution :-
    show_solution_(1, 1).
show_solution_(10, _).
show_solution_(Row, 10) :-
    nl,
    NRow is Row + 1,
    show_solution_(NRow, 1).
show_solution_(_, 4) :-
    write(' |'), fail.
show_solution_(_, 7) :-
    write(' |'), fail.
show_solution_(_, Col) :-
    Col > 1,
    write(' '), fail.
show_solution_(4, 1) :-
    write('------+-------+------'), nl, fail.
show_solution_(7, 1) :-
    write('------+-------+------'), nl, fail.
show_solution_(Row, Col) :-
    (find_chr_constraint(cell(Row-Col, [Value])),
        write(Value)
        ;
        write(' ')),
    (NCol is Col + 1,
    show_solution_(Row, NCol)).


solve(Puzzle_name) :-
    puzzles(P, Puzzle_name),
    show(P), nl,
    initial_store(P),
    propagate,
    show_solution,
    cleanup.
solveall :-
    puzzles(_, Puzzle_name),
    write(Puzzle_name), nl,
    once(time(solve(Puzzle_name))),
    nl,
    fail.
solveall.

initial_store(Puzzle) :-
    initial_store_(Puzzle, 1, _).
initial_store_([], _, _).
initial_store_([Row | RPuzzle], RowI, RowO) :-
    initialize_row(Row, RowI, 1, _),
    RowO is RowI + 1,
    initial_store_(RPuzzle, RowO, _).
initialize_row([], _, _, _).
initialize_row([Value | RRow], Row, ColI, ColO) :-
    nonvar(Value),
    cell(Row-ColI, [Value]),
    rvc(Row-Value, [ColI]),
    ColO is ColI + 1,
    initialize_row(RRow, Row, ColO, _).
initialize_row([NoVal | RRow], Row, ColI, ColO) :-
    var(NoVal),
    cell(Row-ColI, [1,2,3,4,5,6,7,8,9]),
    rvc(Row-1, [ColI]),
    rvc(Row-2, [ColI]),
    rvc(Row-3, [ColI]),
    rvc(Row-4, [ColI]),
    rvc(Row-5, [ColI]),
    rvc(Row-6, [ColI]),
    rvc(Row-7, [ColI]),
    rvc(Row-8, [ColI]),
    rvc(Row-9, [ColI]),
    ColO is ColI + 1,
    initialize_row(RRow, Row, ColO, _).

box(Row-Col, ORow-OCol) :-
    (Row - 1) // 3 =:= (ORow - 1) // 3,
    (Col - 1) // 3 =:= (OCol - 1) // 3.


% Channeling with two viewpoints

consolidate_rvcs @ rvc(Row-Val, [Col]), rvc(Row-Val, Cols) # passive
    <=> rvc(Row-Val, [Col | Cols]).

fill_single @ single(Row, Col, Val), cell(Row-Col, _) # passive,
    rvc(Row-Val, _) # passive
    <=> cell(Row-Col, [Val]), rvc(Row-Val, [Col]).


remove(Row, Col, Val) \ cell(Row-Col, [V1, V2]) # passive
    <=> member(Val, [V1, V2]), member(V, [V1, V2]), V \= Val
        | cell(Row-Col, [V]), single(Row, Col, V).
remove(Row, Col, Val) \ cell(Row-Col, Vs) # passive
    <=> select(Val, Vs, NVs)
        | cell(Row-Col, NVs).
remove(Row, Col, Val) \ rvc(Row-Val, [C1, C2]) # passive
    <=> member(Col, [C1, C2]), member(C, [C1, C2]), C \= Col
        | rvc(Row-Val, [C]), single(Row, C, Val).
remove(Row, Col, Val) \ rvc(Row-Val, Cs) # passive
    <=> select(Col, Cs, NCs)
        | rvc(Row-Val, NCs).
remove(_, _, _) <=> true.


%alldifferent_in_row @ cell(Row-ColA, [Value]), cell(Row-ColB, [Value]) # passive
%    <=> ColA \= ColB | false.
%alldifferent_in_column @ cell(RowA-Col, [Value]),
%    cell(RowB-Col, [Value]) # passive
%    <=> RowA \= RowB | false.
alldifferent_in_box @ cell(Row-Col, [Value]), cell(ORow-OCol, [Value]) # passive
    <=> (Row \= ORow ; Col \= OCol), box(Row-Col, ORow-OCol) | false.


alldifferent_in_row @ propagate \ rvc(Row-Val, [ColA]),
    rvc(Row-Val, [ColB]) # passive
    <=> ColA \= ColB | false.
alldifferent_in_column @ propagate \ rvc(RowA-Val, [Col]),
    rvc(RowB-Val, [Col]) # passive
    <=> RowA \= RowB | false.
%alldifferent_in_box @ propagate \ rvc(Row-Val, [Col]),
%    rvc(ORow-Val, [OCol]) # passive
%    <=> (Row \= ORow ; Col \= OCol), box(Row-Col, ORow-OCol) | false.


eliminate_in_row @ propagate, cell(Row-ColA, [Value]),
    cell(Row-ColB, [_, _ | _])
    ==> ColA \= ColB
        | remove(Row, ColB, Value).
eliminate_in_column @ propagate, cell(RowA-Col, [Value]),
    cell(RowB-Col, [_, _ | _])
    ==> RowA \= RowB
        | remove(RowB, Col, Value).
eliminate_in_box @ propagate, cell(Row-Col, [Value]),
    cell(ORow-OCol, [_, _ | _])
    ==> (Row \= ORow ; Col \= OCol), box(Row-Col, ORow-OCol)
        | remove(ORow, OCol, Value).


same_row_diff_values_eliminate_column @ propagate, rvc(Row-ValA, [Col]),
    rvc(Row-ValB, [_, _ | _])
    ==> ValA \= ValB
        | remove(Row, Col, ValB).
same_value_diff_rows_eliminate_Column @ propagate, rvc(RowA-Val, [Col]),
    rvc(RowB-Val, [_, _ | _])
    ==> RowA \= RowB
        | remove(RowB, Col, Val).


propagate <=> search(2).

first_fail @ cell(Row-Col, Vs) # passive \ search(N)
<=> length(Vs, N) | member(V, Vs), single(Row, Col, V), propagate.

first_fail @ rvc(Row-Val, Cs) # passive \ search(N) 
<=> length(Cs, N) | member(C, Cs), single(Row, C, Val), propagate.

search(9) <=> true.
search(N) <=> NN is N + 1, search(NN).

cleanup \ cell(_, _) <=> true.
cleanup \ rvc(_, _) <=> true.
cleanup <=> true.

