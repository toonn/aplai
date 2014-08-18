:- lib(ic).
%:- import alldifferent/1 from ic_global.
:- import occurrences/3 from ic_global.
:- use_module(library(util)).
:- use_module(library(listut)).

:- compile("sudex_toledo").

% Static stuff

select(S) :-
    member(S,[%input_order,
              %first_fail,
              %smallest,
              %largest,
              %occurrence,
              most_constrained]).
choice(Ch) :-
    member(Ch,[indomain%,
               %indomain_min, indomain_max, indomain_middle,
               %indomain_split,indomain_random
               ]).
method(M) :-
    member(M,[complete
        %,bbs(10)
        %,lds(10)
        %,credit(10)
        ]).
% 0: Nothing, 1: row_col(Board) 2: row_col(SquareSets), 
% 3: occ1(Board), 4: occ1(SquareSets), 5: squares(Blocks)
constraints(C) :-
    member(C,[
        %[1,2,3,0,0],
        [1,0,3,0,5],
        [3,0,1,0,5],
        [1,0,0,0,5],
        [0,0,3,0,5]
        %[1,2,3,4,5]
        ]).

% Top

solveall :-
    solve(_),
    fail.
solveall.

experiment :-
    constraints(Co),write(Co),write(' '),
    select(S),write(S),write(' '),
    choice(Ch),write(Ch),write(' '),
    method(M),write(M),write(' '),nl,
    puzzles(P, Name),write(Name),write(' '),nl,
    time(solution(P,Co,S,Ch,M)),nl,
    fail.
experiment.

solve(Name) :-
    puzzles(P, Name),
    write(Name),nl,
    time(solution(P,[0,0,1,2,3],most_constrained,indomain,[])),
    nl.

solution(Problem,Constraints,Selection,Choice,Method) :- 
    length(Problem,N),
    dim(Board, [N,N]),
    Board[1..N,1..N] :: 1..N,
    sqrt(N,NI), 
    fix(NI,NS),
    dim(Blocks,[N,NS,NS]),
    Blocks[1..N,1..NS,1..NS] :: 1..N,
    % Board - Blocks link
    ( multifor([I,J,K],[1,1,1],[N,NS,NS]), param(Board,Blocks,NS) do 
        rowsForBox(I,NS,RowB,RowE),
        colsForBox(I,NS,ColB,ColE),
        BoxI is Board[RowB..RowE,ColB..ColE],
        nth1(J,BoxI,BRow),nth1(K,BRow,BElemJK),
        #=(Blocks[I,J,K],BElemJK,1)
    ),
    % Other viewpoint
    dim(SquareSets,[N,N]),
    SquareSets[1..N,1..N] :: 1..N,
    % Link Board - SquareSets
    ( multifor([I,J,K],1,N), param(Board,SquareSets) do
        #=(Board[J,K],I,B),
        #=(SquareSets[I,K],J,B)
    ),
    % Fill in values from problem list
    ( multifor([I,J],1,N), param(Problem,Board) do
        nth1(I,Problem,RowI),
        nth1(J,RowI,ElemIJ),
        subscript(Board,[I,J],ElemIJ)
    ),
    % Constraints
    constrain(Board,Blocks,SquareSets,Constraints),
    % Search
    term_variables(Board, Vars),
    search(Vars,0,Selection,Choice,Method,[]).
    %printboard(Board).

constrain(Board,Blocks,SquareSets,Constraints) :-
    ( multifor([I,J],1,5), param(Board,Blocks,SquareSets,Constraints) do
        nth1(J,Constraints,C),
        (I=:=C -> A=J ; A=0),
        (A=:=1 -> row_col_constraint(Board) ; true),
        (A=:=2 -> row_col_constraint(SquareSets) ; true),
        (A=:=3 -> occurs_once_constraint(Board) ; true),
        (A=:=4 -> occurs_once_constraint(SquareSets) ; true),
        (A=:=5 -> squares_constraint(Blocks) ; true)
    ).
        


% Utility

rowsForBox(I,N,RowB,RowE) :-
    RowB is (((I-1) // N)*N)+1,
    RowE is RowB+N-1.

colsForBox(I,N,ColB,ColE) :-
    IN is I-1,
    mod(IN,N,M),
    ColB is M*N+1,
    ColE is ColB+N-1.

removeAllFrom(Dom,List) :-
    write(startraf),nl,
    ( foreach(D,Dom), foreach(L,List) do
        write(D),write(L),nl,
        get_domain_as_list(L,LD),
        write(LD),nl,
        \+ length(LD,2),
        write(yay),
        D #\= L,
        write(yay),nl
    ).

% Print

printboard(Board) :- % Van eclipleclp.org/examples/sudoku.ecl.txt, J. Schimpf
dim(Board, [N,N]),
( for(I,1,N), param(Board,N) do
    ( for(J,1,N), param(Board,I) do
        X is Board[I,J],
        ( var(X) -> write("  _") ; printf(" %2d", [X]) )
    ), nl
), nl.

printboard_(Board) :- %TODO aan de praat krijgen of weg doen
    dim(Board,[N2,N2]),
    sqrt(N2,NI), 
    fix(NI,N),
    ( for(I,0,N-2), param(Board) do
        printblock(Board[N*I+1..(N*I)+N,1..N2],N),
        (for(_,1,N2) do write('-')),
        nl
    ),
    printblock(Board[N2-N+1..N2,1..N2],N,N2).
printblock(BoardBlock,Y) :-
    ( for(I,1,Y), param(BoardBlock,Y) do % Rows
        ( for(J,0,Y-2), param(BoardBlock,I,Y) do % Per row (2x3+1x3)
            ( for(K,1,Y), param(BoardBlock,I,J,Y) do % Per 3
                write(BoardBlock[I,(J*Y)+K])
            ),
            write('|')
        ),
        ( for(L,1,Y), param(BoardBlock,Y,I) do % Per 3
            write(BoardBlock[I,(Y*Y)+L])
        ),
        nl
    ).

% Constraints

row_col_constraint(Board) :-
    dim(Board, [N2,N2]),
    ( for(I,1,N2), param(Board,N2) do
        Row is Board[I,1..N2],
        Col is Board[1..N2,I],
        alldifferent(Row),
        alldifferent(Col)
    ).

squares_constraint(Blocks) :-
    dim(Blocks,[N,NS,NS]),
    ( for(I,1,N), param(Blocks,NS) do
        BlockI is Blocks[I,1..NS,1..NS],
        flatten(BlockI,FBlockI),
        alldifferent(FBlockI)
    ).

occurs_once_constraint(Board) :-
    dim(Board, [N2,N2]),
    ( multifor([I,J],1,N2), param(Board,N2) do
        Row is Board[I,1..N2],
        Col is Board[1..N2,I],
        occurrences(J,Row,1),
        occurrences(J,Col,1)
    ).

naked_pairs_constraint(Board) :-
    dim(Board,[N,N]),
    ( multifor([I,J],1,N), param(Board,N) do
        El is Board[I,J],
        write(El),nl,
        get_domain_as_list(El,ElDom),
        write(ElDom),nl,
        length(ElDom,2),
        write(pair),nl,
        RowT is Board[I,1..N], eclipse_language:delete(El,RowT,Row),
        write(RowT),nl,
        ColT is Board[1..N,J], eclipse_language:delete(El,ColT,Col),
        write(ColT),nl,
        findall(A,
            (member(OEl,Row),write(OEl),nl,
            get_domain_as_list(OEl,ElDom),write(ElDom),nl,
            A = row),
            RFinds),
        findall(A,
            (member(OEl,Col),
            get_domain_as_list(OEl,ElDom),
            A = col),
            CFinds),
        write(RFinds),nl,write(CFinds),
        (foreach(_,RFinds), param(ElDom,Row) do
            removeAllFrom(ElDom,Row)
        ),
        write(m),
        (foreach(_,CFinds), param(ElDom,Col) do 
            removeAllFrom(ElDom,Col)
        ),
        write(m),nl
    ).
        
        
