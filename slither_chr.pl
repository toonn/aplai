:- module(slither_chr, [solve/1, solveall/0]).
:- use_module(library(chr)).
:- chr_option(debug, on). % on - off
:- chr_option(optimize, off). % full - off
:- chr_option(check_guard_bindings, on). % on - off

%:- chr_type list(T) ---> [] ; [T | list(T)].
%:- chr_type row == natural.
%:- chr_type col == natural.
%:- chr_type value == natural.
%:- chr_type pos ---> row-col.
%:- chr_type val --->  [value | list(value)].
%:- chr_type rv ---> value-value.
%
%:- chr_constraint cell(+pos, +val).
%:- chr_constraint rvc(+rv, +list(natural)).
%:- chr_constraint search(+natural).
%:- chr_constraint propagate, cleanup.
%:- chr_constraint single(+row, +col, +value), remove(+row, +col, +value).
%
%:- consult('sudex_toledo.pl').

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
