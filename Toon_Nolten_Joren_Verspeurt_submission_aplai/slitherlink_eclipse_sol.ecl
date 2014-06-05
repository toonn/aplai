:- lib(ic).
%:- import alldifferent/1 from ic_global.
:- import occurrences/3 from ic_global.
:- import sumlist/3 from ic_global.
:- use_module(library(util)).
:- use_module(library(listut)).

:- compile("APLAI_slither_puzzles").

slithersolve(Name) :-
    puzzle(Name,NR,NC,Cells),
    slither(NR,NC,Cells,Sol),
    print_solution(Cells,NR,NC,Sol),
    nl.

slither(NRows,NCols,LONC,ListOfEdges) :-
    % Instantiation
    NRowsP is NRows+1,
    NColsP is NCols+1,
    dim(HorE,[NRowsP,NCols]),
    dim(VertE,[NRows,NColsP]),
    dim(Dots,[NRowsP,NColsP]),
    HorE[1..NRowsP,1..NCols] :: 0..1, % Boolean
    VertE[1..NRows,1..NColsP] :: 0..1, % Boolean
    /*
    * 0: no edges 1: -' 2: -- 3: -, 4: '- 5: | 6: ,-
    */
    Dots[1,1] :: [0,6], Dots[1,2..NCols] :: [0,2,3,6], Dots[1,NColsP] :: [0,3],
    Dots[2..NRows,1] :: [0,4,5,6], Dots[2..NRows,NColsP] :: [0,1,3,5],
    Dots[NRowsP,1] :: [0,4], Dots[NRowsP,2..NCols] :: [0,1,2,4], 
    Dots[NRowsP,NColsP] :: [0,1],
    Dots[2..NRows,2..NCols] :: 0..6,
    % Linking
    ( multifor([I,J],[1,1],[NRowsP,NCols]), param(HorE,Dots) do
        #=(HorE[I,J],1,B), #=(HorE[I,J],0,NB),
        #\=(Dots[I,J],0,B), 
        #\=(Dots[I,J],1,B), #\=(Dots[I,J],2,NB), 
        #\=(Dots[I,J],3,B), #\=(Dots[I,J],4,NB), 
        #\=(Dots[I,J],5,B), #\=(Dots[I,J],6,NB),
        JP is J+1,
        #\=(Dots[I,JP],0,B),
        #\=(Dots[I,JP],1,NB), #\=(Dots[I,JP],2,NB),
        #\=(Dots[I,JP],3,NB), #\=(Dots[I,JP],4,B),
        #\=(Dots[I,JP],5,B),  #\=(Dots[I,JP],6,B)
    ),
    ( multifor([I,J],[1,1],[NRows,NColsP]), param(VertE,Dots) do
        #=(VertE[I,J],1,B), #=(VertE[I,J],0,NB),
        #\=(Dots[I,J],0,B), 
        #\=(Dots[I,J],1,B),  #\=(Dots[I,J],2,B), 
        #\=(Dots[I,J],3,NB), #\=(Dots[I,J],4,B), 
        #\=(Dots[I,J],5,NB), #\=(Dots[I,J],6,NB),
        IP is I+1,
        #\=(Dots[IP,J],0,B),
        #\=(Dots[IP,J],1,NB), #\=(Dots[IP,J],2,B),
        #\=(Dots[IP,J],3,B),  #\=(Dots[IP,J],4,NB),
        #\=(Dots[IP,J],5,NB), #\=(Dots[IP,J],6,B)
    ),
    ( foreach(cell(X,Y,V),LONC), param(VertE,HorE) do
        XP is X+1, YP is Y+1,
        sumlist([HorE[X,Y],HorE[XP,Y],VertE[X,Y],VertE[X,YP]],V)
    ),
    ( multifor([I,J],[1,1],[NRowsP,NCols]), foreach(B,ListOfEdges), 
            param(HorE) do
        B is HorE[I,J]
    ),
    ( multifor([I,J],[1,1],[NRows,NCols]), foreach(B,ListOfEdges),
            param(VertE) do
        B is VertE[I,J]
    ),
    term_variables(Dots, Vars),
    search(Vars,0,most_constrained,indomain,complete,[]).



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







