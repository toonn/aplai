:- lib(ic).
:- import alldifferent/1 from ic_global.

% Strongly inspired by eclipseclp.org/examples/sudoku.ecl.txt

solve(name) :-
    puzzles(P, name),
    write(P),nl,
    solution(P, PSol),
    printboard(P).

solution(Problem, Solution) :-
    length(Problem,N),
    dim(Board, [N,N]),
    Board[1..N,1..N] :: 1..N,
    row_col_constraint(Board,N),
    squares_constraint(Board,N),
    term_variables(Board, Vars),
    labeling(Vars).

printboard(Board) :-
    dim(Board, [N,N]),
    ( for(I,1,N), param(Board,N) do
      ( for(J,1,N), param(Board,I) do
        X is Board[I,J],
        ( var(X) -> write("  _") ; printf(" %2d", [X]) )
      ), nl
    ), nl.

% Constraints

row_col_constraint(Board,N) :-
    ( for(I,1,N), param(Board,N) do
        Row is Board[I,1..N],
        alldifferent(Row),
        Col is Board[1..N,I],
        alldifferent(Col)
    ).

squares_constraint(Board,N) :-
    sqrt(N,NS),
    ( multifor([I,J],1,N,NS), param(Board,NS) do
        ( multifor([K,L],0,N-1), param(Board,I,J), foreach(X,SubSquare) do
            X is Board[I+K,J+L]
        ),
        alldifferent(SubSquare)
    ).
