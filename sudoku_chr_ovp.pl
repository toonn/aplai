:- module(sudoku_chr_ovp, [solve/1, solveall/0]).
:- use_module(library(chr)).
:- chr_option(debug, on). % on - off
:- chr_option(optimize, off). % full - off
:- chr_option(check_guard_bindings, on). % on - off

:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type row == natural.
:- chr_type col == natural.
:- chr_type pos ---> row-col.
:- chr_type val --->  [natural | list(natural)].
:- chr_type rv ---> natural-natural.

:- chr_constraint cell(+pos, +val).
:- chr_constraint rvc(+rv, +list(natural)).
:- chr_constraint search(+natural). 
:- chr_constraint propagate, convert, cleanup.

%foo/1, bar/2, foo(+,?,-list(int)).
% int, dense_int (can be used as array index), float, number, natural, any
% chr_type alias == list(list(int)).

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
    convert,
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
    rvc(Row-Value, [ColI]),
    ColO is ColI + 1,
    initialize_row(RRow, Row, ColO, _).
initialize_row([NoVal | RRow], Row, ColI, ColO) :-
    var(NoVal),
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


% Alternative viewpoint

consolidate_rvcs @ rvc(Row-Val, [Col]), rvc(Row-Val, Cols) # passive
    <=> rvc(Row-Val, [Col | Cols]).


alldifferent_in_row @ propagate \ rvc(Row-Val, [ColA]), rvc(Row-Val, [ColB]) # passive
    <=> ColA \= ColB | false.
alldifferent_in_column @ propagate \ rvc(RowA-Val, [Col]), rvc(RowB-Val, [Col]) # passive
    <=> RowA \= RowB | false.
alldifferent_in_box @ propagate \ rvc(Row-Val, [Col]), rvc(ORow-Val, [OCol]) # passive
    <=> (Row \= ORow ; Col \= OCol), box(Row-Col, ORow-OCol) | false.

same_row_diff_values_eliminate_column @ propagate, rvc(Row-ValA, [Col])
    \ rvc(Row-ValB, [C1, C2 | Cs])
    <=> ValA \= ValB, select(Col, [C1, C2 | Cs], NCs)
        | rvc(Row-ValB, NCs).
same_value_diff_rows_eliminate_Column @ propagate, rvc(RowA-Val, [Col])
    \ rvc(RowB-Val, [C1, C2 | Cs])
    <=> RowA \= RowB, select(Col, [C1, C2 | Cs], NCs)
        | rvc(RowB-Val, NCs).


propagate <=> search(2).

first_fail @ search(N), rvc(Row-Val, Cs) # passive
<=> length(Cs, N) | member(C, Cs), rvc(Row-Val, [C]), propagate.

search(9) <=> true.
search(N) <=> NN is N + 1, search(NN).

convert \ rvc(Row-Val, [Col]) <=> cell(Row-Col, [Val]), convert.
convert <=> true.

cleanup \ cell(_, _) <=> true.
cleanup <=> true.

