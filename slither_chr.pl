:- module(slither_chr, [solve/1, solveall/0]).
:- use_module(library(chr)).
:- chr_option(debug, on). % on - off
:- chr_option(optimize, off). % full - off
:- chr_option(check_guard_bindings, on). % on - off

%:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type row == natural.
:- chr_type col == natural.
:- chr_type fill ---> 0 ; 1.
:- chr_type edge_degree ---> 0 ; 1 ; 2 ; 3.
:- chr_type point ---> p(row, col).

:- chr_constraint c(+row, +col, +edge_degree).
:- chr_constraint h(+row, +col, ?fill).
:- chr_constraint v(+row, +col, ?fill).
:- chr_constraint segment(+point, +point).
:- chr_constraint search.
%:- chr_constraint propagate, cleanup.
%:- chr_constraint single(+row, +col, +value), remove(+row, +col, +value).
%
:- consult('APLAI_slither_puzzles.ecl').

edge_index(h(R,C),_,N,Index) :- 
    var(Index), !, Index is (R-1)*N + (C-1).
edge_index(v(R,C),M,N,Index) :- 
    var(Index), !, Index is (R-1)*(N+1) + (C-1) + (M+1)*N.
edge_index(Edge,M,N,Index) :- 
    nonvar(Index),
    ( Index >= (M+1)*N -> 
        Index2 is Index - (M+1)*N,
        Col is Index2 mod (N+1) + 1,
        Row is Index2 div (N+1) + 1,
        Edge = v(Row,Col)
    ;
        Col is Index mod N + 1,
        Row is Index div N + 1,
        Edge = h(Row,Col)
    ).

print_solution(Cells,M,N,Solution) :-
    print_row(1,M,N,Solution,Cells).
    
print_hvalue(X) :- var(X), write('+   '), !.
print_hvalue(1) :- write('+---'), !.
print_hvalue(0) :- write('+ x '), !.

print_vvalue(X) :- var(X), write('    '), !.
print_vvalue(1) :- write('|   '), !.
print_vvalue(0) :- write('    '), !.

print_vvaluem(X) :- var(X), write(' '), !.
print_vvaluem(1) :- write('|'), !.
print_vvaluem(0) :- write('x'), !.

print_cell(R,C,Cells) :- 
    ( member(cell(R,C,V),Cells) -> 
        write(' '), write(V), write(' ')
    ; 
        write('   ')
    ).    

print_row(R,M,N,Values,Cells) :-
    N2 is N + 1,
    forall( ( between(1,N ,C), 
              edge_index(h(R,C),M,N,Index), 
              nth0(Index,Values,Value) ), 
             ( print_hvalue(Value) ) ), write('+'), nl,
    forall( ( between(1,N2,C), 
              edge_index(v(R,C),M,N,Index), 
              nth0(Index,Values,Value) ), 
             ( print_vvalue(Value) ) ), nl,
    forall( ( between(1,N2,C), 
              edge_index(v(R,C),M,N,Index), 
              nth0(Index,Values,Value) ), 
             ( print_vvaluem(Value), print_cell(R,C,Cells) ) ), nl,
    forall( ( between(1,N2,C), 
              edge_index(v(R,C),M,N,Index), 
              nth0(Index,Values,Value) ), 
             ( print_vvalue(Value) ) ), nl,        
    R2 is R + 1,
    ( R2 =< M + 1 ->
        print_row(R2,M,N,Values,Cells)
    ;
        true
    ).

slither(NRows, NCols, NumberedCells, Solution) :-
    mkedges(NRows, NCols),
    mkcells(NumberedCells),
    %propagate,
    search,
    show_solution(NumberedCells, NRows, NCols, Solution).
    %cleanup.

solve(Puzzle) :-
    puzzle(Puzzle, NRows, NCols, NumberedCells),
    write(Puzzle), nl,
    slither(NRows, NCols, NumberedCells, _).

solveall :-
    puzzle(Puzzle, _, _, _),
    Puzzle \= a1,
    once(time(solve(Puzzle))),
    nl,
    fail.
solveall.

show_solution(Cells, Rows, Cols, Sol) :-
    gather_sol(Rows, Cols, Sol),
    print_solution(Cells, Rows, Cols, Sol).

gather_sol(Rows, Cols, Solution) :-
    NRows is Rows + 1,
    NCols is Cols + 1,
    gather_vs(Rows, NCols, [], VSol),
    gather_hs(NRows, Cols, VSol, Solution).

gather_vs(0, _, Vs, Vs).
gather_vs(Rows, Cols, Tail, Vs) :-
    gather_v_row(Rows, Cols, Tail, NTail),
    NRows is Rows - 1,
    gather_vs(NRows, Cols, NTail, Vs).

gather_v_row(_, 0, Vs, Vs).
gather_v_row(RowIndex, Cols, Tail, Vs) :-
    find_chr_constraint(v(RowIndex, Cols, F)),
    NCols is Cols - 1,
    gather_v_row(RowIndex, NCols, [F | Tail], Vs).

gather_hs(0, _, Hs, Hs).
gather_hs(Rows, Cols, Tail, Vs) :-
    gather_h_row(Rows, Cols, Tail, NTail),
    NRows is Rows - 1,
    gather_hs(NRows, Cols, NTail, Vs).

gather_h_row(_, 0, Vs, Vs).
gather_h_row(RowIndex, Cols, Tail, Vs) :-
    find_chr_constraint(h(RowIndex, Cols, F)),
    NCols is Cols - 1,
    gather_h_row(RowIndex, NCols, [F | Tail], Vs).

mkcells([]).
mkcells([cell(R, C, N) | NumberedCells]) :-
    c(R, C, N),
    mkcells(NumberedCells).

mkedges(Rows, Cols) :-
    NRows is Rows + 1,
    NCols is Cols + 1,
    mkhors(NRows, Cols),
    mkvers(Rows, NCols).

mkhors(0, _) :- !.
mkhors(Rows, Cols) :-
    FalseCol is Cols + 1,
    h(Rows, 0, 0),
    h(Rows, FalseCol, 0),
    mkhor(Rows, Cols),
    NRows is Rows - 1,
    mkhors(NRows, Cols).

mkhor(_, 0) :- !.
mkhor(RowIndex, Cols) :-
    h(RowIndex, Cols, _),
    NCols is Cols - 1,
    mkhor(RowIndex, NCols).

mkvers(_, 0) :- !.
mkvers(Rows, Cols) :-
    FalseRow is Rows + 1,
    v(0, Cols, 0),
    v(FalseRow, Cols, 0),
    mkver(Rows, Cols),
    NCols is Cols - 1,
    mkvers(Rows, NCols).

mkver(0, _) :- !.
mkver(Rows, ColIndex) :-
    v(Rows, ColIndex, _),
    NRows is Rows - 1,
    mkver(NRows, ColIndex).

% Constraint Handling Rules

% Single Cycle constraint
create_hor_segment @ h(R, C, 1)
    ==> NC is C + 1
        | segment(p(R, C), p(R, NC)).
create_ver_segment @ v(R, C, 1)
    ==> NR is R + 1
        | segment(p(R, C), p(NR, C)).
merge_segments_1=2 @ segment(A, B), segment(B, C)
    <=> segment(A, C).
merge_segments_1=1 @ segment(A, B), segment(A, C)
    <=> segment(B, C).
merge_segments_2=2 @ segment(A, B), segment(C, B)
    <=> segment(A, C).

closed_loop_and_another_segment_fail @ segment(P, P), segment(_, _)
    <=> fail.

segment(P, P), c(_, _, _) <=> fail.

segment(P, P), h(_, _, F) ==> var(F) | F = 0.
segment(P, P), v(_, _, F) ==> var(F) | F = 0.


% Cell Number constraints
cell_0_no_filled_edges @ h(R, C, HF1), h(NR, C, HF2), v(R, C, VF1),
    v(R, NC, VF2) \ c(R, C, 0)
    <=> NR is R + 1, NC is C + 1
        | HF1 = 0, HF2 = 0, VF1 = 0, VF2 = 0.

cell_1_no_filled_edges @ h(R, C, HF1), h(NR, C, HF2),
    v(R, C, VF1), v(R, NC, VF2) \ c(R, C, 1)
    <=> NR is R + 1, NC is C + 1
        | (HF1 = 1, HF2 = 0, VF1 = 0, VF2 = 0
            ;
           HF1 = 0, HF2 = 1, VF1 = 0, VF2 = 0
            ;
           HF1 = 0, HF2 = 0, VF1 = 1, VF2 = 0
            ;
           HF1 = 0, HF2 = 0, VF1 = 0, VF2 = 1).

cell_2_no_filled_edges @ h(R, C, HF1), h(NR, C, HF2),
    v(R, C, VF1), v(R, NC, VF2) \ c(R, C, 2)
    <=> NR is R + 1, NC is C + 1
        | (HF1 = 1, HF2 = 1, VF1 = 0, VF2 = 0
            ;
           HF1 = 1, HF2 = 0, VF1 = 1, VF2 = 0
            ;
           HF1 = 1, HF2 = 0, VF1 = 0, VF2 = 1
            ;
           HF1 = 0, HF2 = 1, VF1 = 1, VF2 = 0
            ;
           HF1 = 0, HF2 = 1, VF1 = 0, VF2 = 1
            ;
           HF1 = 0, HF2 = 0, VF1 = 1, VF2 = 1).

cell_3_no_filled_edges @ h(R, C, HF1), h(NR, C, HF2),
    v(R, C, VF1), v(R, NC, VF2) \ c(R, C, 3)
    <=> NR is R + 1, NC is C + 1
        | (HF1 = 0, HF2 = 1, VF1 = 1, VF2 = 1
            ;
           HF1 = 1, HF2 = 0, VF1 = 1, VF2 = 1
            ;
           HF1 = 1, HF2 = 1, VF1 = 0, VF2 = 1
            ;
           HF1 = 1, HF2 = 1, VF1 = 1, VF2 = 0).


% Degree of every node is either 0 or 2
degree_0_or_2 @ h(R, C, E), h(R, NC, W),
    v(R, C, S), v(NR, C, N)
    ==> NR is R - 1, NC is C - 1,
        ((nonvar(N), nonvar(E), N =:= 1, E =:= 1
            ;
          nonvar(N), nonvar(S), N =:= 1, S =:= 1
            ;
          nonvar(N), nonvar(W), N =:= 1, W =:= 1
            ;
          nonvar(E), nonvar(S), E =:= 1, S =:= 1
            ;
          nonvar(E), nonvar(W), E =:= 1, W =:= 1
            ;
          nonvar(S), nonvar(W), S =:= 1, W =:= 1)

            ;

        ((nonvar(N) , nonvar(E) , nonvar(S))
            ;
        (nonvar(N) , nonvar(E) , nonvar(W))
            ;
        (nonvar(N) , nonvar(S) , nonvar(W))
            ;
        (nonvar(E) , nonvar(S) , nonvar(W))))
        | (N = 0, E = 0, S = 0, W = 0
            ;
           N = 1, E = 1, S = 0, W = 0
            ;
           N = 1, E = 0, S = 1, W = 0
            ;
           N = 1, E = 0, S = 0, W = 1
            ;
           N = 0, E = 1, S = 1, W = 0
            ;
           N = 0, E = 1, S = 0, W = 1
            ;
           N = 0, E = 0, S = 1, W = 1).

% Depth First Search
depth_first @ search, segment(_, p(R, C)), h(R, C, E), h(R, NC, W), v(R, C, S),
    v(NR, C, N)
    ==> NR is R - 1, NC is C - 1
        | (N = 1, E = 1, S = 0, W = 0
            ;
           N = 1, E = 0, S = 1, W = 0
            ;
           N = 1, E = 0, S = 0, W = 1
            ;
           N = 0, E = 1, S = 1, W = 0
            ;
           N = 0, E = 1, S = 0, W = 1
            ;
           N = 0, E = 0, S = 1, W = 1).
