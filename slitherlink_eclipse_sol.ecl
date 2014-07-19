:- lib(ic).
:- import sumlist/2 from ic_global.
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
    write(link),nl,

    % Cycle viewpoint
    dim(CyMemb,[NRowsP,NColsP]),
    MaxCycles is NRowsP*NColsP/4,
    CyMemb[1..NRowsP,1..NColsP] :: 0..MaxCycles,
    ( multifor([I,J],[1,1],[NRowsP,NColsP]), param(NRowsP,NColsP,Dots,CyMemb) do
        DIJ is Dots[I,J], CyIJ is CyMemb[I,J],
        IMU is I-1, IPU is I+1, JMU is J-1, JPU is J+1,
        bwrp(IMU,1,NRowsP,IM), bwrp(IPU,1,NRowsP,IP),
        bwrp(JMU,1,NColsP,JM), bwrp(JPU,1,NColsP,JP),
        CyUp is CyMemb[IM,J], CyDown is CyMemb[IP,J], 
        CyLeft is CyMemb[I,JM], CyRight is CyMemb[I,JP], 
        #=(DIJ,0,A), #=(CyIJ,0,A),
        #=(DIJ,3,Is3), #=(DIJ,5,Is5),
        #=(DIJ,9,Is9), #=(DIJ,6,Is6),
        #=(DIJ,10,Is10), #=(DIJ,12,Is12),
        #=(CyLeft,CyIJ,IsLeft), #=(CyUp,CyIJ,IsUp),
        #=(CyRight,CyIJ,IsRight), #=(CyDown,CyIJ,IsDown),
        IsLeft #>= Is3, IsLeft #>= Is5, IsLeft #>= Is9,
        IsUp #>= Is3, IsUp #>= Is6, IsUp #>= Is10,
        IsRight #>= Is5, IsRight #>= Is6, IsRight #>= Is12,
        IsDown #>= Is9, IsDown #>= Is10, IsDown #>= Is12
    ),
    write(cy),nl,

    % Constrain
    ( foreach(cell(X,Y,V),LONC), param(HorE,VertE) do
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
    % Search
    term_variables(Dots, Vars),
    search(Vars,0,first_fail,indomain,complete,[]),
    write(search),nl,
    labelCycles(CyMemb,1,1,0).
    %singleCycle(Dots,HorE,VertE).

bwrp(I,Lo,Hi,Hi) :-
    I<Lo,!.
bwrp(I,Lo,Hi,Lo) :-
    I>Hi,!.
bwrp(I,_,_,I).

labelCycles(Cy,I,J,_) :-
    dim(Cy,[I,J]),
    !. %Convenience contra-IOOB
labelCycles(Cy,I,J,L) :-
    dim(Cy,[_,M]),
    D is Cy[I,J],
    nonvar(D),
    rectNext(I,J,M,IN,JN),
    labelCycles(Cy,IN,JN,L).
labelCycles(Cy,I,J,0) :-
    dim(Cy,[_,M]),
    D is Cy[I,J],
    var(D),
    D #= 1,
    rectNext(I,J,M,IN,JN),
    labelCycles(Cy,IN,JN,1).

% Check if the grid contains 1 complete cycle by going over all non-zero dots
singleCycle(Dots,HorE,VertE) :-
    filterNonZero(Dots,1,1,[],[Start|NonZero]),
    %length([Start|NonZero],L),
    %write(L),
    sc(NonZero,[Start],HorE,VertE).
    %,nl.

filterNonZero(Dots,I,J,T,R) :-
    dim(Dots,[I,J]),
    H is Dots[I,J],
    (
        ( H=\=0, R=[[I,J]|T] )
    ;
        ( H==0, R=T )
    ),
    !.
    %,nl. % Convenience to prevent index out of bounds in next case
filterNonZero(Dots,I,J,T,R) :-
    dim(Dots,[_,M]),
    rectNext(I,J,M,IN,JN),
    H is Dots[I,J],
    (
        ( H==0, filterNonZero(Dots,IN,JN,T,R) )
    ;
        ( H=\=0, filterNonZero(Dots,IN,JN,[[I,J]|T],R) )
    ).

rectNext(I,M,M,IN,1) :-
    !, % Convenience
    IN is I+1.
rectNext(I,J,_,I,JN) :-
    JN is J+1.

sc([],_,_,_) :- !. % Don't backtrack on a found single cycle.
sc(NZ,[H|T],Hor,Vert) :-
    member(N,NZ),
    connected(H,N,Hor,Vert),
    !, % Don't go into the other way round for the 1st node.
    delete(N,NZ,NZN),
    sc(NZN,[N,H|T],Hor,Vert).

%AB
connected([I,JA],[I,JB],H,_) :-
    JA =:= JB-1,
    HD is H[I,JA],
    HD==1,
    %write('- '),
    !. % Performance
%BA
connected([I,JA],[I,JB],H,_) :-
    JA =:= JB+1,
    HD is H[I,JB],
    HD==1,
    %write(' -'),
    !. % Performance
%A
%B
connected([IA,J],[IB,J],_,V) :-
    IA =:= IB-1,
    VD is V[IA,J],
    VD==1,
    %write(','),
    !. % Performance
%B
%A
connected([IA,J],[IB,J],_,V) :-
    IA =:= IB+1,
    VD is V[IB,J],
    VD==1.%,
    %write('\'').
    

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
