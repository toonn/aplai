:- lib(ic).
:- import time/1 from util.
:- import nth0/3 from listut.

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
    write(inst),nl,

    % Set up domains
    HorE[1..NRowsP,1..NCols] :: 0..1, % Boolean
    VertE[1..NRows,1..NColsP] :: 0..1, % Boolean
    %Mid
    Dots[2..NRows,2..NCols] :: [0,3,5,6,9,10,12], % Vals: W 1, N 2, E 4, S 8
    %Top,Bot
    Dots[1,2..NCols] :: [0,5,9,12], Dots[NRowsP,2..NCols] :: [0,3,5,6],
    %Left,Right
    Dots[2..NRows,1] :: [0,6,10,12], Dots[2..NRows,NColsP] :: [0,3,9,10],
    %TL,TR
    Dots[1,1] :: [0,12], Dots[1,NColsP] :: [0,9], 
    %BL,BR
    Dots[NRowsP,1] :: [0,6], Dots[NRowsP,NColsP] :: [0,3],
    write(doms),nl,

    % Link
    %Mid
    ( multifor([I,J],[2,2],[NRows,NCols]), param(HorE,VertE,Dots) do
        IM is I-1, JM is J-1,
        Dots[I,J] #= HorE[I,JM] + 2*VertE[IM,J] + 4*HorE[I,J] + 8*VertE[I,J]
    ),
    %Top
    ( multifor([I,J],[1,2],[1,NRows]), param(HorE,VertE,Dots) do
        JM is J-1,
        Dots[I,J] #= HorE[I,JM] + 4*HorE[I,J] + 8*VertE[I,J]
    ),
    %Bottom
    ( multifor([I,J],[NRowsP,2],[NRowsP,NCols]), param(HorE,VertE,Dots) do
        IM is I-1, JM is J-1,
        Dots[I,J] #= HorE[I,JM] + 2*VertE[IM,J] + 4*HorE[I,J]
    ),
    %Left
    ( multifor([I,J],[2,1],[NRows,1]), param(HorE,VertE,Dots) do
        IM is I-1,
        Dots[I,J] #= 2*VertE[IM,J] + 4*HorE[I,J] + 8*VertE[I,J]
    ),
    %Right
    ( multifor([I,J],[2,NColsP],[NRows,NColsP]), param(HorE,VertE,Dots) do
        IM is I-1, JM is J-1,
        Dots[I,J] #= HorE[I,JM] + 2*VertE[IM,J] + 8*VertE[I,J]
    ),
    %Corners
    Dots[1,1] #= 4*HorE[1,1] + 8*VertE[1,1],
    Dots[1,NColsP] #= HorE[1,NCols] + 8*VertE[1,NColsP],
    Dots[NRowsP,1] #= 2*VertE[NRows,1] + 4*VertE[NRows,1],
    Dots[NRowsP,NColsP] #= HorE[NRowsP,NCols] + 2*VertE[NRows,NColsP],
    %reverse
    %( multifor([i,j],[1,1],[nrowsp,ncols]), param(hore,dots) do
    %    JP is J+1, H is HorE[I,J], D is Dots[I,J], DP is Dots[I,JP],
    %    #=(6,D,A1),  #=(12,D,A2), #=(5,D,A3),  #=(A1+A2+A3,H),
    %    #=(3,DP,B1), #=(9,DP,B2), #=(5,DP,B3), #=(B1+B2+B3,H)
    %),
    %( multifor([I,J],[1,1],[NRows,NColsP]), param(VertE,Dots) do
    %    IP is I+1, V is VertE[I,J], D is Dots[I,J], DP is Dots[IP,J],
    %    #=(3,D,A1),  #=(6,D,A2),   #=(10,D,A3),  #=(A1+A2+A3,V),
    %    #=(9,DP,B1), #=(12,DP,B2), #=(10,DP,B3), #=(B1+B2+B3,V)
    %),
    write(link),nl,

    % Constrain
    ( foreach(cell(X,Y,V),LONC), param(HorE,VertE) do
        write(cell(X,Y,V)),nl,
        XP is X+1, YP is Y+1,
        HorE[X,Y] + HorE[XP,Y] + VertE[X,Y] + VertE[X,YP] #= V
    ),
    write(cons),nl,

    % Put together return
    ( multifor([I,J],[1,1],[NRowsP,NCols]), foreach(B,ListOfEdges1), 
            param(HorE) do
        B is HorE[I,J]
    ),
    ( multifor([I,J],[1,1],[NRows,NColsP]), foreach(B,ListOfEdges2),
            param(VertE) do
        B is VertE[I,J]
    ),
    append(ListOfEdges1,ListOfEdges2,ListOfEdges),
    write_term(Dots,[depth(full)]),nl,
    % Search
    term_variables(Dots, Vars),
    search(Vars,0,first_fail,indomain,complete,[]).

% Print helpers

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
    ( for(C,1,N), param(Values,R,M,N) do
        edge_index(h(R,C),M,N,Index), 
        nth0(Index,Values,Value), 
        print_hvalue(Value) 
    ), write('+'),nl,
    ( for(C,1,N2), param(Values,R,M,N) do
        edge_index(v(R,C),M,N,Index), 
        nth0(Index,Values,Value), 
        print_vvalue(Value) 
    ), nl,
    ( for(C,1,N2), param(Values,R,M,N,Cells) do
        edge_index(v(R,C),M,N,Index), 
        nth0(Index,Values,Value), 
        print_vvaluem(Value),
        print_cell(R,C,Cells)
    ), nl,
    ( for(C,1,N2), param(Values,R,M,N) do
        edge_index(v(R,C),M,N,Index), 
        nth0(Index,Values,Value), 
        print_vvalue(Value)
    ), nl,
    R2 is R + 1,
    ( R2 =< M + 1 ->
        print_row(R2,M,N,Values,Cells)
    ;
        true
    ).
