:- module(sudoku_chr_channeling, [solve/1, solveall/0]).
:- use_module(library(chr)).
:- chr_option(debug, on). % on - off
:- chr_option(optimize, off). % full - off
:- chr_option(check_guard_bindings, on). % on - off

:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type row == natural.
:- chr_type col == natural.
:- chr_type pos ---> row-col.
:- chr_type val ---> [natural | list(natural)].

:- chr_constraint single(+natural, +pos).
:- chr_constraint val_set(+natural, +list(pos), +list(pos)).
:- chr_constraint cell(+pos, +val).
:- chr_constraint search(+natural), search2(+natural). 
:- chr_constraint propagate, propagate2, cleanup.

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
    once(solve(Puzzle_name)),
    nl,
    fail.
solveall.

initial_store(Puzzle) :-
    initial_sets(9),
    initial_store_(Puzzle, 1, _).
initial_sets(0) :- !.
initial_sets(Set) :-
    all_pos(All),
    val_set(Set, [], All),
    NSet is Set - 1,
    initial_sets(NSet).
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
initialize_row([Value | RRow], Row, ColI, ColO) :-
    var(Value),
    cell(Row-ColI, [1,2,3,4,5,6,7,8,9]),
    ColO is ColI + 1,
    initialize_row(RRow, Row, ColO, _).

all_pos(All) :-
    all_pos_(1,1,All).
all_pos_(9, 9, [9-9]) :- !.
all_pos_(I, 9, [I-9 | All]) :-
    !,
    II is I + 1,
    all_pos_(II, 1, All).
all_pos_(I, J, [I-J | All]) :-
    JJ is J + 1,
    all_pos_(I, JJ, All).

box(Row-Col, ORow-OCol) :-
    (Row - 1) // 3 =:= (ORow - 1) // 3,
    (Col - 1) // 3 =:= (OCol - 1) // 3.

influence(Row-_, Row-_).
influence(_-Col, _-Col).
influence(P, OP) :-
    box(P, OP).

% Channeling constraints

cell(P, [V]), val_set(V, FPs, Ps)
    <=> \+ member(P, FPs), \+ member(P, Ps), fail.
cell(P, [V]), val_set(V, FPs, _), propagate
    ==> \+ member(P, FPs), single(V, P).
cell(P, Vs) \ val_set(V, FPs, Ps)
    <=> \+ member(V, Vs), select(P, Ps, NPs), val_set(V, FPs, NPs).
cell(P, [_]) \ cell(P, _) <=> true.
val_set(V, _, Ps) \ cell(P, Vs)
    <=> \+member(P, Ps), select(V, Vs, NVs), cell(P, NVs).

% Classic viewpoint

alldifferent_in_row @ cell(Row-ColA, [Value]), cell(Row-ColB, [Value]) # passive
    <=> ColA \= ColB | false.
alldifferent_in_column @ cell(RowA-Col, [Value]),
        cell(RowB-Col, [Value]) # passive
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

% Alternative viewpoint

fail_when_no_more_positions @ val_set(_, FPs, Ps)
    <=> length(FPs, L1), (L1 > 9 ; length(Ps, L2), L1+L2 < 9) | fail.

finish_set @ val_set(V, FPs, [_|_])
    <=> length(FPs, 9) | val_set(V, FPs, []).

complete_set_when_just_enough_possible @ propagate, val_set(V, FPs, [P | Ps])
    ==> length(FPs, L1), length([P | Ps], L2), L1 + L2 =:= 9
            | single(V, P).

remove_possible_position @ single(V, P) \ val_set(OV, OFPs, Ps)
    <=> V \= OV, select(P, Ps, NPs) | val_set(OV, OFPs, NPs).

fill_in_position @ single(V, P), val_set(V, FPs, Ps)
    <=> exclude(influence(P), Ps, NPs), val_set(V, [P | FPs], NPs),
        cell(P, [V]). % <- channeling 'constraint'


% Search

propagate <=> search(2).

first_fail @ search(N), cell(Row-Col, Vs) # passive
<=> length(Vs, Len), Len =:= N | member(V, Vs), cell(Row-Col, [V]), propagate.

search(9) <=> true.
search(N) <=> NN is N + 1, search(NN).


cleanup \ val_set(_, _, _) <=> true.
cleanup \ cell(_, _) <=> true.
cleanup <=> true.

