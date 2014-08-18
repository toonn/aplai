:- lib(ic).
:- import sumlist/2 from ic_global.
:- lib(util).
:- import nth0/3 from listut.

:- compile("APLAI_slither_puzzles").

solve(Name) :-
    puzzle(Name,NR,NC,Cells),
    slither(NR,NC,Cells,Sol,[vars(dots),sc(cymemb),squares(off)]),
    \+ print_solution(Cells,NR,NC,Sol),
    nl.

% A selection of puzzles from the APLAI_slither_puzzles.ecl file
pnames( 5,[a5,b5,janko5b,janko5c,janko5d,janko5e]).
pnames(10,[a10,b10,61,62,63,861,103,105]).
pnames(12,[janko12a]).

experiment(Size) :-
    pnames(Size,Names),
    member(Name,Names),
    write(Name),nl,
    puzzle(Name,Size,Size,Cells),
    member(Squares,[on,off]),
    member(Vars,[squares,dots]),
    member(SC,[cymemb,explicit]),
    member(Sel,
        [input_order,first_fail,smallest,largest,occurrence,most_constrained]
    ),
    member(Choice,
        [indomain,indomain_min,indomain_max,indomain_random]
    ),
    time(slither(Size,Size,Cells,_,
        [vars(Vars),sc(SC),squares(Squares),search(Sel,Choice)]
    )),
    nl,
    fail.

slither(NRows,NCols,LONC,ListOfEdges,Opts) :-
    % Options:
    % vars( squares | dots )
    % sc( cymemb | explicit )
    % squares( on | off )
    % [debug]
    % [consp]
    (
        (
            \+ memberchk(vars(squares),Opts),!
        ; 
            memberchk(squares(on),Opts)
        ),
        write(Opts),!
    ;
        write('Inconsistent options'),
        fail
    ),

    % Instantiation
    NRowsP is NRows+1,
    NColsP is NCols+1,
    dim(HorE,[NRowsP,NCols]),
    dim(VertE,[NRows,NColsP]),
    dim(Dots,[NRowsP,NColsP]),
    (memberchk(debug,Opts) -> write(inst),nl ; true),

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
    (memberchk(debug,Opts) -> write(doms),nl ; true),

    % Link
    %Mid
    ( multifor([I,J],[2,2],[NRows,NCols]), param(HorE,VertE,Dots) do
        IM is I-1, JM is J-1,
        Dots[I,J] #= HorE[I,JM] + 2*VertE[IM,J] + 4*HorE[I,J] + 8*VertE[I,J]
    ),
    %Top
    ( multifor([I,J],[1,2],[1,NCols]), param(HorE,VertE,Dots) do
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
    (memberchk(debug,Opts) -> write(link),nl ; true),

    % Cycle viewpoint
    ( memberchk(sc(cymemb),Opts) ->
        dim(CyMemb,[NRowsP,NColsP]),
        MaxCycles is NRowsP*NColsP/4,
        CyMemb[1..NRowsP,1..NColsP] :: 0..MaxCycles,
        ( multifor([I,J],[1,1],[NRowsP,NColsP]), 
                param(NRowsP,NColsP,Dots,CyMemb) do
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
        (memberchk(debug,Opts) -> write(cy),nl ; true)
    ;
        true
    ),

    % Squares viewpoint
    dim(Squares,[NRows,NCols]),
    % 0: 0 1: wnes 2: (wn)(ne)(es)(sw) hor vert 3: wnes => 0..14
    Squares[1..NRows,1..NCols] :: 0..14,

    % Constrain
    ( foreach(cell(X,Y,V),LONC), param(HorE,VertE,Squares) do
        XP is X+1, YP is Y+1,
        HorE[X,Y] + HorE[XP,Y] + VertE[X,Y] + VertE[X,YP] #= V,
        SXY is Squares[X,Y],
        #=(V,1,One), #::(SXY,1..4,One),
        #=(V,2,Two), #::(SXY,5..10,Two),
        #=(V,3,Three), #::(SXY,11..14,Three)
    ),
    (memberchk(debug,Opts) -> write(cons),nl ; true),
    
    ( memberchk(squares(on),Opts) ->
        ( multifor([I,J],[1,1],[NRows,NCols]), param(Squares,HorE,VertE) do
            SIJ is Squares[I,J], IP is I+1, JP is J+1,
            L is VertE[I,J], R is VertE[I,JP], T is HorE[I,J], B is HorE[IP,J],
            L+T+R+B #= Sum,
            #=(Sum,0,Zero), #=(SIJ,0,Zero),
            #=(Sum,1,One), One #= (SIJ #>= 1 and SIJ #< 5),
            #=(Sum,2,Two), Two #= (SIJ #>= 5 and SIJ #< 11),
            #=(Sum,3,Three), #>=(SIJ,11,Three),
            #=(SIJ,1,S1), #=(SIJ,2,S2), #=(SIJ,3,S3), #=(SIJ,4,S4),
            #=(SIJ,5,S5), #=(SIJ,6,S6), #=(SIJ,7,S7), #=(SIJ,8,S8),
            #=(SIJ,9,S9), #=(SIJ,10,S10),
            #=(SIJ,11,S11), #=(SIJ,12,S12), #=(SIJ,13,S13), #=(SIJ,14,S14),
            S1+S5+S8+S10+S12+S13+S14 #= L,
            S2+S5+S6+S9+S11+S13+S14 #= T,
            S3+S6+S7+S10+S11+S12+S14 #= R,
            S4+S7+S8+S9+S11+S12+S13 #= B,
            #=(neg(L)+neg(T)+neg(R)+neg(B),4,S0),
            #=(L+neg(T)+neg(R)+neg(B),4,S1),
            #=(neg(L)+T+neg(R)+neg(B),4,S2),
            #=(neg(L)+neg(T)+R+neg(B),4,S3),
            #=(neg(L)+neg(T)+neg(R)+B,4,S4),
            #=(L+T+neg(R)+neg(B),4,S5),
            #=(neg(L)+T+R+neg(B),4,S6),
            #=(neg(L)+neg(T)+R+B,4,S7),
            #=(L+neg(T)+neg(R)+B,4,S8),
            #=(neg(L)+T+neg(R)+B,4,S9),
            #=(L+neg(T)+R+neg(B),4,S10),
            #=(neg(L)+T+R+B,4,S11),
            #=(L+neg(T)+R+B,4,S12),
            #=(L+T+neg(R)+B,4,S13),
            #=(L+T+R+neg(B),4,S14)
        ),
        % 3-3 patterns
        ( multifor([I,J],[1,1],[NRows,NCols]), param(Squares,NRows,NCols) do
            IP is I+1, JP is J+1, JM is J-1, 
            This is Squares[I,J], 
            #>=(This,11,T), #>=(Right,11,R), #>=(Bottom,11,B),
            #>=(BotLeft,11,BL), #>=(BotRight,11,BR),
            % |3|3|
            (JP =< NCols ->
                Right is Squares[I,JP], 
                #=(T+R,2,TR), 
                TR => (This :: [12,14] and Right :: [12,14])
                % Slows execution down CONSIDERABLY
                %TR => (   (#=(This,12) => #=(Right,14)) 
                %      and (#=(This,14) => #=(Right,12)) )
                ;
                    true
                ),
            (IP =< NRows ->
                % '3-3,
                Bottom is Squares[IP,J],
                #=(T+B,2,TB), 
                TB => (This :: [11,13] and Bottom :: [11,13]),
                % Slows execution down CONSIDERABLY
                %TR => (   (#=(This,11) => #=(Bottom,13)) 
                %      and (#=(This,13) => #=(Bottom,11)) )

                % 3/3
                (JM >= 1 ->
                    BotLeft is Squares[IP,JM], 
                    #=(T+BL,2,TBL), 
                    TBL => (This :: [11,14] and BotLeft :: [12,13])
                ;
                    true
                ),
                % 3\3
                (JP =< NCols ->
                    BotRight is Squares[IP,JP],
                    #=(T+BR,2,TBR), 
                    TBR => (This :: [13,14] and BotRight :: [11,12])
                ;
                    true
                )
            ;
                true
            )
        ),    
        (member(debug,Opts) -> write(squares),nl ; true)
    ;
        true
    ),
    
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
    (memberchk(debug,Opts) -> write_array(Squares) ; true),
    (memberchk(consp,Opts) -> 
        \+ print_solution(LONC,NRows,NCols,ListOfEdges)
    ; true),
    % Search
    term_variables(Squares, SVars),
    term_variables(Dots, DVars),
    (memberchk(vars(squares),Opts) -> Vars = SVars ; Vars = DVars),
    (memberchk(search(Sel,Choice),Opts) -> 
        true 
    ; 
        Sel = most_constrained, Choice = indomain_max
    ),
    search(Vars,0,Sel,Choice,complete,[]),
    (member(debug,Opts) -> write(search),nl ; true),
    ( memberchk(sc(cymemb),Opts) ->
        labelCycles(CyMemb,1,1,0)
    ;
        singleCycle(Dots,HorE,VertE)
    ).

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
    sc(NonZero,[Start],HorE,VertE).

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

write_array(A) :-
    dim(A,[M,_]),
    ( for(I,1,M), param(A) do
        E is A[I],
        write_term(E,[depth(full)]),nl
    ).

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
