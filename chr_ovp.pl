:- module(sudoku_chr_ovp, [solve/1, solveall/0]).
:- use_module(library(chr)).
:- chr_option(debug, off). % on - off
:- chr_option(optimize, full). % full - off
:- chr_option(check_guard_bindings, off). % on - off

:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type row == natural.
:- chr_type col == natural.
:- chr_type pos ---> row-col.
:- chr_type val ---> [natural | list(natural)].

:- chr_constraint single(+natural, +pos).
:- chr_constraint filled(+list(pos)).
:- chr_constraint remove(+pos, +list(natural)).
:- chr_constraint val_set(+natural, +list(pos)).
:- chr_constraint cell(+pos, +val).
:- chr_constraint search(+natural). 
:- chr_constraint propagate, convert, cleanup.

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
    !,
    propagate/*,
    convert,
    show_solution,
    cleanup*/.
solveall :-
    puzzles(_, Puzzle_name),
    time(once(solve(Puzzle_name))),
    nl,
    fail.
solveall.

initial_store(Puzzle) :-
    initial_sets(9),
    initial_store_(Puzzle, 1, _),
    filled([]).
initial_sets(0) :- !.
initial_sets(Set) :-
    all_pos(All),
    val_set(Set, All),
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
    single(Value, Row-ColI),
    ColO is ColI + 1,
    initialize_row(RRow, Row, ColO, _).
initialize_row([Value | RRow], Row, ColI, ColO) :-
    var(Value),
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

mkcells(_, []).
mkcells(V, [P | Ps]) :-
    cell(P, [V]),
    mkcells(V, Ps).
    
combination(0, _, []).
combination(N, [P | Ps], [P | RComb]) :-
    N > 0,
    NN is N - 1,
    combination(NN, Ps, RComb).
combination(N, [_ | Ps], Combination) :-
    N > 0,
    combination(N, Ps, Combination).

non_essential_pos(R-C, Pos) :-
    combination(3, Pos, [R1-C1, R2-C2, R3-C3]),
    (R == R1 ; R == R2 ; R == R3),
    (C == C1 ; C == C2 ; C == C3),
    (box(R-C, R1-C1) ; box(R-C, R2-C2) ; box(R-C, R3-C3)),
    !.
remove_non_essential_pos(Pos, NPos) :-
    select(RemP, Pos, NPos),
    non_essential_pos(RemP, NPos).

essential_pos_(_,[],[]).
essential_pos_(Seen, [ER-EC | UnSeen], [ER-EC | EPos]) :-
    (\+ ((member(R-_, Seen) ; member(R-_, UnSeen)), ER == R)
        ;
        \+ ((member(_-C, Seen) ; member(_-C, UnSeen)), EC == C)
        ;
        \+ ((member(P, Seen) ; member(P, UnSeen)), box(ER-EC, P))),
    !,
    essential_pos_([ER-EC | Seen], UnSeen, EPos).
essential_pos_(Seen, [NEP | UnSeen], EPos) :-
    essential_pos_([NEP | Seen], UnSeen, EPos).
essential_pos(Pos, EPos) :-
    essential_pos_([], Pos, EPos).

fill_essential_(_, [], _).
fill_essential_(V, [EP | EPos], Fs) :-
    member(EP, Fs),
    fill_essential_(V, EPos, Fs).
fill_essential_(V, [EP | EPos], Fs) :-
    single(V, EP),
    fill_essential_(V, EPos, Fs).
fill_essential(V, EPos, Fs) :-
    fill_essential_(V, EPos, Fs),
    propagate.


% Alternative viewpoint

fail_when_two_sets_require_same_position @ val_set(V1, Pos1), val_set(V2, Pos2)
    <=> V1 \= V2, essential_pos(Pos1, EPos1), essential_pos(Pos2, EPos2),
        intersection(EPos1, EPos2, Intersection), \+ length(Intersection, 0)
        | fail.


remove(_, []) <=> true.
remove_single_from_other @ remove(Psingle, [V | Vs]),
    val_set(V, Pos)
    <=> (select(Psingle, Pos, NPos) ; Pos = NPos)
        | val_set(V, NPos), remove(Psingle, Vs).
fill_single @ propagate \ single(V, Psingle),
    filled(Fs), val_set(V, Pos)
    <=> exclude(influence(Psingle), Pos, NPos),
        select(V, [1,2,3,4,5,6,7,8,9], Vs)
        | filled([Psingle | Fs]), remove(Psingle, Vs),
            val_set(V, [Psingle | NPos]).



propagate <=> search(8).

first_fail @ val_set(V, Pos) # passive, filled(Fs) # passive \ search(N)
    <=> essential_pos(Pos, EPos), length(EPos, N)
        | fill_essential(V, EPos, Fs).

search(0) <=> true.
search(N) <=> NN is N - 1, search(NN).



set_to_cell_representation @ convert \ val_set(V, Pos)
    <=> mkcells(V, Pos).
remove_uncertain_cells @ convert \ cell(P, [V]), cell(P, [H | Vs])
    <=> cell(P, [V, H | Vs]).
convert <=> true.
cleanup \ cell(_, _) <=> true.
cleanup <=> true.

