:- module(sudoku_chr_ovp2, [solve/1, solveall/0]).
:- use_module(library(chr)).
:- chr_option(debug, off). % on - off
:- chr_option(optimize, full). % full - off
:- chr_option(check_guard_bindings, off). % on - off

:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type row == natural.
:- chr_type col == natural.
:- chr_type pos ---> row-col.
:- chr_type val --->  [natural | list(natural)].

:- chr_constraint cell(+pos, +val).
:- chr_constraint search(+natural). 
:- chr_constraint propagate, cleanup.

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
    ColO is ColI + 1,
    initialize_row(RRow, Row, ColO, _).
initialize_row([NoVal | RRow], Row, ColI, ColO) :-
    var(NoVal),
    cell(Row-ColI, [1,2,3,4,5,6,7,8,9]),
    ColO is ColI + 1,
    initialize_row(RRow, Row, ColO, _).

box(Row-Col, ORow-OCol) :-
    /*
        %between(1, 9, ORow),
        %between(1, 9, OCol),
        RowBase is div(Row - 1, 3),
        ORowBase is div(ORow - 1, 3),
        RowBase =:= ORowBase,
        ColBase is div(Col - 1, 3),
        OColBase is div(OCol - 1, 3),
        ColBase =:= OColBase.
    */
    (Row - 1) // 3 =:= (ORow - 1) // 3,
    (Col - 1) // 3 =:= (OCol - 1) // 3.


% Classical viewpoint

alldifferent_in_row @ cell(Row-ColA, [Value]), cell(Row-ColB, [Value]) # passive
    <=> ColA \= ColB | false.
alldifferent_in_column @ cell(RowA-Col, [Value]), cell(RowB-Col, [Value]) # passive
    <=> RowA \= RowB | false.
alldifferent_in_box @ cell(Row-Col, [Value]), cell(ORow-OCol, [Value]) # passive
    <=> (Row \= ORow ; Col \= OCol), box(Row-Col, ORow-OCol) | false.

eliminate_in_row @ propagate, cell(Row-ColA, [Value])
    \ cell(Row-ColB, [V1, V2 | Vs])
    <=> ColA \= ColB, select(Value, [V1, V2 | Vs], NVs)
        | cell(Row-ColB, NVs).
eliminate_in_column @ propagate, cell(RowA-Col, [Value])
    \ cell(RowB-Col, [V1, V2 | Vs])
    <=> RowA \= RowB, select(Value, [V1, V2 | Vs], NVs)
        | cell(RowB-Col, NVs).
eliminate_in_box @ propagate, cell(Row-Col, [Value])
    \ cell(ORow-OCol, [V1, V2 | Vs])
    <=> (Row \= ORow ; Col \= OCol), box(Row-Col, ORow-OCol),
        select(Value, [V1, V2 | Vs], NVs)
        | cell(ORow-OCol, NVs).

/*
single_position_row @ cell(Row-Col, [V1, V2 | Vs])
    <=> member(V, [V1, V2 | Vs]), Col \= OCol,
            cell(Row-OCol, OVs), member(V, OVs)
        | \+ member(V, OVs), cell(Row-Col, [V]).
single_position_column @ cell(Row-Col, [V1, V2 | Vs])
    <=> member(V, [V1, V2 | Vs]), Row \= ORow,
            cell(ORow-Col, OVs), member(V, OVs)
        | \+ member(V, OVs), cell(Row-Col, [V]).
single_position_box @ cell(Row-Col, [V1, V2 | Vs])
    <=> member(V, [V1, V2 | Vs]), (Row \= ORow ; Col \= OCol),
            box(Row-Col, ORow-OCol), cell(ORow-OCol, OVs), member(V, OVs)
        | \+ member(V, OVs), cell(Row-Col, [V]).
*/


propagate <=> search(2).

first_fail @ search(N), cell(Row-Col, Vs) # passive
<=> length(Vs, Len), Len =:= N | member(V, Vs), cell(Row-Col, [V]), propagate.

search(9) <=> true.
search(N) <=> NN is N + 1, search(NN).

cleanup \ cell(_, _) <=> true.
cleanup <=> true.

