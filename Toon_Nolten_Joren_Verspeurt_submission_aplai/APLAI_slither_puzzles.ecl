% Convert Janko puzzles to a format our solution accepts
convertjanko(Number,Size,puzzle(Number,Size,Size,List)) :-
    janko(Number,Size,Strings),
    cj(Strings,1,[],List).

cj([],_,List,List).
cj([H|T],Row,Acc,List) :-
    cj_(H,Row,1,Acc,NAcc),
    NRow is Row+1,
    cj(T,NRow,NAcc,List).

cj_([],_,_,List,List).
cj_([-|T],R,C,Acc,List) :-
    NC is C+1,
    cj_(T,R,NC,Acc,List),
    !.
cj_([H|T],R,C,Acc,List) :-
    NC is C+1,
    cj_(T,R,NC,[cell(R,C,H)|Acc],List).

janko(61,10,[
[2,-,-,-,2,0,-,3,-,0],
[-,3,-,0,-,-,3,-,3,-],
[1,-,3,1,-,-,3,-,3,-],
[-,1,-,-,0,-,-,1,2,-],
[3,-,1,-,2,-,2,3,-,3],
[-,-,0,-,-,-,3,1,-,-],
[2,2,-,-,1,1,-,-,3,1],
[3,3,-,1,-,-,-,2,1,3],
[-,-,0,-,-,-,2,-,-,-],
[-,3,3,-,1,2,-,-,3,-]]).
janko(62,10,[
[3,-,2,1,-,3,2,2,-,1],
[-,3,-,-,0,-,3,1,-,3],
[3,-,1,-,2,-,3,-,3,-],
[3,-,3,-,-,-,3,-,-,3],
[3,-,1,2,-,3,-,0,-,-],
[-,3,-,-,2,-,0,-,1,3],
[1,-,-,0,-,-,2,-,2,-],
[-,-,2,3,2,1,-,-,2,-],
[-,3,-,-,1,-,3,-,0,3],
[3,-,-,1,1,-,0,-,3,-]]).
janko(63,10,[
[-,1,1,3,-,-,2,1,3,-],
[2,-,-,-,-,-,-,-,-,0],
[3,-,-,1,3,1,0,-,-,1],
[2,-,3,-,-,-,-,3,-,1],
[-,-,3,-,2,2,-,0,-,-],
[-,-,3,-,2,3,-,2,-,-],
[1,-,1,-,-,-,-,2,-,0],
[1,-,-,3,2,3,2,-,-,1],
[3,-,-,-,-,-,-,-,-,1],
[-,1,1,0,-,-,3,2,3,-]]).
janko(861,10,[
[-,2,1,2,-,-,-,-,1,-],
[-,1,-,1,2,1,2,3,2,-],
[-,2,-,2,-,2,-,-,1,-],
[-,3,2,1,-,-,-,-,2,1],
[-,2,-,-,-,2,1,2,1,-],
[-,1,2,1,-,3,-,-,2,-],
[-,-,3,-,-,2,3,-,1,2],
[2,1,2,3,2,1,-,-,-,1],
[-,2,-,2,-,2,3,2,1,2],
[-,1,-,1,-,-,-,-,-,-]]).
janko(103,10,[
[-,1,1,-,1,-,-,-,-,1],
[-,1,1,-,-,1,1,1,1,-],
[-,1,1,-,-,-,1,1,-,1],
[1,1,-,-,1,-,-,1,-,1],
[1,1,1,-,-,1,1,1,1,-],
[-,-,1,-,-,1,-,-,1,-],
[-,1,-,1,-,-,-,1,1,-],
[-,1,1,1,-,1,1,-,-,-],
[1,1,-,-,1,1,1,1,1,1],
[-,-,-,1,1,-,-,-,1,-]]).
janko(105,10,[
[1,-,2,-,2,-,1,1,1,-],
[1,-,2,-,2,-,2,-,2,2],
[2,-,2,2,2,2,2,-,-,-],
[2,2,-,-,2,2,2,2,2,-],
[-,2,-,-,-,2,2,2,2,3],
[-,2,2,2,2,2,2,1,-,2],
[2,2,2,2,2,-,2,-,-,-],
[-,2,-,-,2,3,-,2,3,-],
[2,3,2,2,-,1,2,-,-,2],
[-,-,2,2,3,-,-,1,1,-]]).


% Trivial puzzles
puzzle(z1,1,1,[cell(1,1,0)]).
puzzle(a1,1,1,[cell(1,1,4)]).
puzzle(ar2,1,2,[cell(1,1,3),cell(1,2,3)]).
puzzle(a2,2,2,[cell(1,1,2),cell(1,2,2),cell(2,1,2),cell(2,2,2)]).
puzzle(b2,2,2,[cell(1,1,2),cell(1,2,2),cell(2,1,2)]).
puzzle(a3,3,3,[cell(1,1,2),cell(1,3,2),cell(2,2,0),cell(3,1,2),cell(3,3,2)]).
puzzle(b3,3,3,[cell(1,1,2),cell(1,3,2),cell(3,1,2),cell(3,3,2)]).

% Own puzzles
%puzzle(janko5,5,5,CL) :-
%    member(Name,[janko5b,janko5c,janko5d,janko5e]),
%    write(Name),nl,
%    puzzle(Name,5,5,CL).

%puzzle(janko10,Size,Size,List) :-
%    convertjanko(Number,10,puzzle(_,_,_,List)),
%    write(Number),nl.
%puzzle(Number,Size,Size,List) :-
%    convertjanko(Number,Size,puzzle(_,_,_,List)).


puzzle(janko5b,5,5,[cell(1,3,2),cell(1,4,2),cell(2,1,3),cell(2,3,2),cell(2,4,1),cell(4,4,3),cell(4,5,1),cell(5,1,0),cell(5,2,2),cell(5,4,2),cell(5,5,3)]).

puzzle(janko5c,5,5,[cell(1,2,2),cell(1,3,3),cell(1,4,2),cell(2,4,0),cell(3,1,2),
cell(3,2,1),cell(3,5,2),cell(4,1,1),cell(4,3,1),cell(4,5,1),cell(5,3,1),
cell(5,4,1),cell(5,5,0)]).

puzzle(janko5d,5,5,[cell(1,2,2),cell(2,1,2),cell(2,2,3),cell(3,4,2),cell(4,1,2),
cell(4,2,2),cell(4,5,3),cell(5,2,3),cell(5,5,3)]).

puzzle(janko5e,5,5,[cell(1,2,1),cell(1,5,0),cell(2,1,1),cell(2,5,1),cell(3,1,1),
cell(3,3,3),cell(4,1,2),cell(4,2,2),cell(4,4,2),cell(5,2,2),cell(5,3,2),
cell(5,4,3)]).

%puzzle(janko12a,12,12,[cell(1,2,3),cell(1,3,3),cell(1,4,3),cell(1,5,1),
%cell(1,7,3),cell(1,8,1),cell(1,10,2),cell(1,12,1),cell(2,2,0),cell(2,6,2),
%cell(2,10,0),cell(2,12,2),cell(3,1,3),cell(3,8,0),cell(3,12,3),cell(4,1,3),
%cell(4,3,0),cell(4,5,3),cell(4,7,3),cell(4,10,2),cell(4,11,3),cell(4,12,2),
%cell(5,1,1),cell(5,9,1),cell(5,10,1),cell(6,4,1),cell(6,5,1),cell(6,12,2),
%cell(7,1,2),cell(7,8,3),cell(7,9,3),cell(8,3,0),cell(8,4,3),cell(8,12,2),
%cell(9,1,2),cell(9,2,0),cell(9,3,2),cell(9,6,2),cell(9,8,2),cell(9,10,2),
%cell(9,12,1),cell(10,1,3),cell(10,5,2),cell(10,12,2),cell(11,1,2),cell(11,3,0),
%cell(11,7,2),cell(11,11,2),cell(12,1,1),cell(12,3,2),cell(12,5,1),cell(12,6,2),
%cell(12,8,2),cell(12,9,1),cell(12,10,3),cell(12,11,2)]).
%
%puzzle(janko12b,12,12,[cell(1,2,1),cell(1,4,3),cell(1,6,3),cell(1,7,3),
%cell(1,8,2),cell(1,9,3),cell(1,10,3),cell(1,12,1),cell(2,3,2),cell(2,12,1),cell(3,1,3),cell(3,4,2),cell(3,7,2),cell(3,10,3),cell(4,1,3),cell(4,6,3),
%cell(4,10,3),cell(4,11,2),cell(5,1,1),cell(5,4,3),cell(5,6,2),cell(5,9,2),
%cell(5,10,2),cell(6,1,2),cell(6,3,2),cell(7,10,1),cell(7,12,3),cell(8,3,3),
%cell(8,4,2),cell(8,7,3),cell(8,9,3),cell(8,12,1),cell(9,2,3),cell(9,3,2),
%cell(9,8,0),cell(9,12,0),cell(10,3,2),cell(10,6,3),cell(10,9,3),cell(10,12,2),
%cell(11,1,1),cell(11,10,3),cell(12,1,2),cell(12,3,3),cell(12,4,1),cell(12,5,2),
%cell(12,6,3),cell(12,7,2),cell(12,9,2),cell(12,11,1)]).

% Example puzzles
puzzle(a5,5,5,[cell(1,2,1), cell(2,1,3), cell(2,5,2), cell(3,1,3), cell(3,2,0), cell(3,3,2), cell(3,5,1), cell(4,1,3), cell(4,2,2), cell(4,3,2), cell(5,3,2)]).
puzzle(b5,5,5,[cell(1,2,2),cell(1,3,1),cell(1,5,3),cell(2,2,2),cell(2,3,2),cell(2,4,3),cell(3,2,2),cell(3,3,2),cell(3,4,1),cell(3,5,2),cell(4,4,1),cell(4,5,2)]).

% Non-unique puzzle.
puzzle(x5,5,5,[cell(1,2,1), cell(2,1,3), cell(2,5,2), cell(3,1,3), cell(3,2,0), cell(3,3,2), cell(3,5,1), cell(4,1,3), cell(4,2,2), cell(4,3,2)]).

puzzle(b7,7,7,[ 
cell(1,1,3),cell(1,2,1),                        cell(1,5,3),cell(1,6,1),
            cell(2,2,2),cell(2,3,1),cell(2,4,1),                        cell(2,7,3),
cell(3,1,1),cell(3,2,2),cell(3,3,2),            cell(3,5,2),            cell(3,7,3),
                                    cell(4,4,3),            cell(4,6,2),
                        cell(5,3,1),cell(5,4,2),cell(5,5,2),
            cell(6,2,3),            cell(6,4,3),cell(6,5,2),            cell(6,7,2),
cell(7,1,2),cell(7,2,2),                        cell(7,5,2),cell(7,6,2),cell(7,7,2)
]).

puzzle(a10,10,10,[cell(1,1,2),cell(1,3,3),cell(1,4,2),cell(2,2,2),cell(2,4,1),cell(2,6,2),cell(2,7,1),cell(2,8,3),cell(2,10,3),cell(3,3,3),cell(3,5,2),cell(3,7,2),cell(3,8,2),cell(4,1,2),cell(4,2,2),cell(4,3,1),cell(4,4,2),cell(4,5,2),cell(4,6,2),cell(4,7,2),cell(4,8,3),cell(5,1,2),cell(5,3,2),cell(5,5,2),cell(5,6,2),cell(5,10,2),cell(6,2,2),cell(6,5,1),cell(6,6,1),cell(6,7,3),cell(7,1,2),cell(7,4,1),cell(7,8,2),cell(7,10,3),cell(8,1,3),cell(8,3,3),cell(8,5,2),cell(8,7,2),cell(8,9,0),cell(9,1,1),cell(9,2,2),cell(9,4,3),cell(9,6,3),cell(9,9,2),cell(10,1,3),cell(10,7,3),cell(10,8,2),cell(10,9,2)]).
puzzle(b10,10,10,[cell(1,1,3),cell(1,4,0),cell(1,8,2),cell(1,9,1),cell(1,10,3),cell(2,2,2),cell(2,6,3),cell(3,1,0),cell(3,7,2),cell(3,10,1),cell(4,1,0),cell(4,3,2),cell(4,6,3),cell(4,7,1),cell(4,9,3),cell(5,2,3),cell(5,4,3),cell(5,5,2),cell(5,6,3),cell(5,7,2),cell(5,9,2),cell(5,10,2),cell(6,1,3),cell(6,3,2),cell(6,5,1),cell(7,1,2),cell(7,2,2),cell(7,3,2),cell(7,4,2),cell(7,5,3),cell(7,6,3),cell(8,1,3),cell(8,4,2),cell(8,6,1),cell(8,8,2),cell(9,1,2),cell(9,3,1),cell(9,8,2),cell(10,3,3),cell(10,5,3),cell(10,6,2),cell(10,8,3),cell(10,9,3)]).
puzzle(a15,15,15,[cell(1,1,3),cell(1,3,3),cell(1,4,3),cell(1,5,3),cell(1,8,2),cell(1,10,2),cell(1,12,2),cell(1,13,2),cell(1,14,2),cell(2,6,1),cell(2,9,3),cell(2,12,2),cell(2,14,3),cell(3,2,2),cell(3,3,2),cell(3,4,3),cell(3,5,3),cell(3,7,2),cell(3,8,2),cell(3,10,3),cell(3,13,1),cell(3,14,2),cell(3,15,3),cell(4,1,3),cell(4,2,2),cell(4,3,3),cell(4,5,2),cell(4,7,2),cell(4,9,3),cell(4,11,1),cell(5,2,2),cell(5,3,2),cell(5,9,2),cell(5,13,2),cell(5,15,3),cell(6,4,1),cell(6,8,1),cell(6,9,3),cell(6,11,1),cell(7,7,1),cell(7,11,1),cell(7,12,2),cell(7,13,1),cell(7,14,2),cell(8,1,2),cell(8,8,3),cell(8,14,2),cell(8,15,0),cell(9,1,1),cell(9,2,2),cell(9,3,1),cell(9,5,2),cell(9,6,2),cell(9,8,1),cell(9,11,3),cell(9,12,0),cell(9,14,3),cell(10,1,3),cell(10,2,3),cell(10,3,3),cell(10,4,1),cell(10,5,2),cell(10,9,2),cell(10,14,0),cell(11,1,2),cell(11,9,2),cell(11,11,2),cell(11,14,2),cell(11,15,3),cell(12,6,3),cell(12,8,1),cell(12,9,1),cell(12,11,1),cell(13,1,3),cell(13,3,2),cell(13,4,3),cell(13,5,1),cell(13,8,3),cell(13,10,2),cell(13,11,2),cell(13,13,2),cell(13,14,3),cell(14,8,2),cell(14,10,3),cell(14,11,2),cell(14,15,2),cell(15,2,2),cell(15,3,1),cell(15,4,3),cell(15,5,2),cell(15,6,1),cell(15,7,3),cell(15,11,3),cell(15,12,2),cell(15,14,3)]).
puzzle(b15,15,15,[cell(1,1,3),cell(1,2,2),cell(1,3,2),cell(1,4,3),cell(1,5,3),cell(1,10,3),cell(1,11,1),cell(1,12,2),cell(1,14,2),cell(2,1,2),cell(2,3,1),cell(2,5,0),cell(2,6,2),cell(2,7,1),cell(2,8,3),cell(2,9,2),cell(2,10,2),cell(2,13,1),cell(3,4,3),cell(3,5,2),cell(3,8,2),cell(3,9,2),cell(3,10,2),cell(3,11,2),cell(3,15,2),cell(4,4,2),cell(4,6,1),cell(4,7,2),cell(4,8,2),cell(4,15,1),cell(5,1,3),cell(5,2,2),cell(5,4,1),cell(5,5,2),cell(5,7,3),cell(5,11,2),cell(5,14,2),cell(5,15,2),cell(6,1,2),cell(6,2,1),cell(6,3,1),cell(6,4,1),cell(6,7,1),cell(6,8,2),cell(6,13,2),cell(6,14,2),cell(7,1,2),cell(7,2,1),cell(7,3,2),cell(7,7,2),cell(7,8,2),cell(7,10,2),cell(7,12,2),cell(7,13,1),cell(8,1,2),cell(8,2,3),cell(8,3,2),cell(8,5,3),cell(8,6,2),cell(8,8,3),cell(8,11,2),cell(8,13,2),cell(8,15,2),cell(9,2,2),cell(9,4,2),cell(9,5,2),cell(9,7,2),cell(9,8,1),cell(9,9,1),cell(9,10,3),cell(9,15,3),cell(10,1,3),cell(10,2,3),cell(10,4,3),cell(10,5,3),cell(10,12,2),cell(11,1,2),cell(11,2,2),cell(11,3,1),cell(11,5,0),cell(11,9,2),cell(11,12,3),cell(12,2,2),cell(12,3,2),cell(12,4,3),cell(12,5,3),cell(12,6,3),cell(12,7,3),cell(12,8,2),cell(12,9,3),cell(12,12,2),cell(12,13,2),cell(12,14,2),cell(12,15,3),cell(13,8,1),cell(13,11,2),cell(13,12,1),cell(13,13,2),cell(13,14,1),cell(13,15,2),cell(14,5,1),cell(14,8,3),cell(14,9,2),cell(14,10,3),cell(14,12,3),cell(15,2,3),cell(15,3,3),cell(15,4,3),cell(15,5,2),cell(15,6,3),cell(15,7,1),cell(15,9,2),cell(15,13,1),cell(15,14,2),cell(15,15,2)]).
puzzle(a20,20,20,[cell(1,1,3),cell(1,3,2),cell(1,4,3),cell(1,6,3),cell(1,14,2),cell(1,15,3),cell(1,17,2),cell(1,18,3),cell(1,20,3),cell(2,4,2),cell(2,5,2),cell(2,9,1),cell(2,11,1),cell(2,13,2),cell(2,14,3),cell(3,3,1),cell(3,4,1),cell(3,6,1),cell(3,7,2),cell(3,8,0),cell(3,9,2),cell(3,11,2),cell(3,12,1),cell(3,15,1),cell(3,19,3),cell(4,1,3),cell(4,4,3),cell(4,10,1),cell(4,11,3),cell(4,13,3),cell(4,14,2),cell(4,15,2),cell(4,16,3),cell(4,17,1),cell(4,18,2),cell(5,1,3),cell(5,2,1),cell(5,3,2),cell(5,4,3),cell(5,9,2),cell(5,12,2),cell(5,15,2),cell(5,16,2),cell(6,3,2),cell(6,6,2),cell(6,7,2),cell(6,10,2),cell(6,13,2),cell(6,17,3),cell(6,18,2),cell(7,1,1),cell(7,2,2),cell(7,4,3),cell(7,5,2),cell(7,6,2),cell(7,7,2),cell(7,8,1),cell(7,9,3),cell(7,11,3),cell(7,15,1),cell(7,19,3),cell(7,20,2),cell(8,1,3),cell(8,5,2),cell(8,6,2),cell(8,10,2),cell(8,12,1),cell(8,13,2),cell(8,15,2),cell(8,16,2),cell(9,1,1),cell(9,4,2),cell(9,11,2),cell(9,14,1),cell(9,16,2),cell(9,17,2),cell(9,19,3),cell(9,20,3),cell(10,1,2),cell(10,4,1),cell(10,5,3),cell(10,6,2),cell(10,8,2),cell(10,10,2),cell(10,11,3),cell(10,13,1),cell(10,17,3),cell(10,18,2),cell(10,20,1),cell(11,1,2),cell(11,3,3),cell(11,4,1),cell(11,6,2),cell(11,7,3),cell(11,9,1),cell(11,12,3),cell(11,15,2),cell(11,16,2),cell(11,17,1),cell(11,20,2),cell(12,1,3),cell(12,9,3),cell(12,10,2),cell(12,15,2),cell(12,16,2),cell(12,17,2),cell(12,19,2),cell(13,2,2),cell(13,4,1),cell(13,5,3),cell(13,11,3),cell(13,15,2),cell(13,18,1),cell(13,20,3),cell(14,1,3),cell(14,4,2),cell(14,6,2),cell(14,7,2),cell(14,9,1),cell(14,11,2),cell(14,12,1),cell(14,13,2),cell(14,15,2),cell(14,19,2),cell(14,20,2),cell(15,3,3),cell(15,4,1),cell(15,5,1),cell(15,6,2),cell(15,7,1),cell(15,8,3),cell(15,12,1),cell(15,13,1),cell(15,16,2),cell(15,17,3),cell(15,18,1),cell(15,20,2),cell(16,1,2),cell(16,2,1),cell(16,3,2),cell(16,4,2),cell(16,7,3),cell(16,9,2),cell(16,13,2),cell(16,14,2),cell(16,15,2),cell(16,16,2),cell(16,18,3),cell(16,20,3),cell(17,1,2),cell(17,2,2),cell(17,3,2),cell(17,5,1),cell(17,7,2),cell(17,8,1),cell(17,12,2),cell(17,16,2),cell(17,17,1),cell(17,18,2),cell(17,20,2),cell(18,1,2),cell(18,5,2),cell(18,6,2),cell(18,7,2),cell(18,10,2),cell(18,11,3),cell(18,15,2),cell(18,16,2),cell(18,17,1),cell(19,1,1),cell(19,4,2),cell(19,9,2),cell(19,10,2),cell(19,13,2),cell(19,14,1),cell(19,17,2),cell(19,18,3),cell(19,20,3),cell(20,3,3),cell(20,6,3),cell(20,7,2),cell(20,11,3),cell(20,13,2),cell(20,17,2),cell(20,18,2),cell(20,19,2)]).

puzzle(big,30,25,[cell(1,2,2),cell(1,3,3),cell(1,5,2),cell(1,6,3),cell(1,11,2),cell(1,12,3),cell(1,13,3),cell(1,15,2),cell(1,17,3),cell(1,22,1),cell(1,24,2),cell(2,1,2),cell(2,5,2),cell(2,6,1),cell(2,8,1),cell(2,10,1),cell(2,11,1),cell(2,14,2),cell(2,15,3),cell(2,16,3),cell(2,18,2),cell(2,19,2),cell(2,20,2),cell(2,21,3),cell(2,23,3),cell(3,1,0),cell(3,2,1),cell(3,4,2),cell(3,8,2),cell(3,10,1),cell(3,12,3),cell(3,14,2),cell(3,17,1),cell(3,18,2),cell(3,22,1),cell(3,23,1),cell(3,24,3),cell(4,1,0),cell(4,2,2),cell(4,6,2),cell(4,10,2),cell(4,11,3),cell(4,12,2),cell(4,13,1),cell(4,14,2),cell(4,16,3),cell(4,17,2),cell(4,18,2),cell(4,20,2),cell(4,22,3),cell(5,2,2),cell(5,4,3),cell(5,5,2),cell(5,8,1),cell(5,11,1),cell(5,13,1),cell(5,14,2),cell(5,17,2),cell(5,18,2),cell(5,24,3),cell(6,3,3),cell(6,8,2),cell(6,11,2),cell(6,14,3),cell(6,16,2),cell(6,17,1),cell(6,21,3),cell(6,22,3),cell(6,25,3),cell(7,1,2),cell(7,4,1),cell(7,6,3),cell(7,7,2),cell(7,8,2),cell(7,9,2),cell(7,11,1),cell(7,16,1),cell(7,17,1),cell(7,21,2),cell(7,22,1),cell(7,23,3),cell(8,1,1),cell(8,2,1),cell(8,4,1),cell(8,5,3),cell(8,6,2),cell(8,9,2),cell(8,11,2),cell(8,15,2),cell(8,17,2),cell(8,19,2),cell(8,22,2),cell(8,24,1),cell(9,2,3),cell(9,7,1),cell(9,8,2),cell(9,10,2),cell(9,11,3),cell(9,12,1),cell(9,13,1),cell(9,14,2),cell(9,15,3),cell(9,17,3),cell(9,21,1),cell(9,23,2),cell(9,24,2),cell(10,5,2),cell(10,6,2),cell(10,7,2),cell(10,9,2),cell(10,13,2),cell(10,18,2),cell(10,19,2),cell(10,20,2),cell(10,24,3),cell(10,25,2),cell(11,1,3),cell(11,3,3),cell(11,4,1),cell(11,6,2),cell(11,7,2),cell(11,14,2),cell(11,15,2),cell(11,18,2),cell(11,20,2),cell(11,22,2),cell(11,23,2),cell(12,1,2),cell(12,3,2),cell(12,4,1),cell(12,7,2),cell(12,8,2),cell(12,9,1),cell(12,10,2),cell(12,14,0),cell(12,15,1),cell(12,18,2),cell(12,19,1),cell(12,20,3),cell(12,22,3),cell(12,24,3),cell(12,25,2),cell(13,3,2),cell(13,7,2),cell(13,8,2),cell(13,9,3),cell(13,11,3),cell(13,14,2),cell(13,18,1),cell(13,23,2),cell(14,2,2),cell(14,4,1),cell(14,5,3),cell(14,14,1),cell(14,16,2),cell(14,18,1),cell(14,22,2),cell(14,23,2),cell(14,25,3),cell(15,1,2),cell(15,2,1),cell(15,8,3),cell(15,13,3),cell(15,15,2),cell(15,17,3),cell(15,19,3),cell(15,21,2),cell(15,22,3),cell(15,25,3),cell(16,3,3),cell(16,4,1),cell(16,5,3),cell(16,6,2),cell(16,8,1),cell(16,10,2),cell(16,11,2),cell(16,12,3),cell(16,13,0),cell(16,14,2),cell(16,15,2),cell(16,16,2),cell(16,17,2),cell(16,21,2),cell(16,23,2),cell(16,24,2),cell(16,25,1),cell(17,1,2),cell(17,2,2),cell(17,4,3),cell(17,6,2),cell(17,7,2),cell(17,8,2),cell(17,9,2),cell(17,10,2),cell(17,12,2),cell(17,15,3),cell(17,17,2),cell(17,19,3),cell(17,24,2),cell(18,1,3),cell(18,2,2),cell(18,7,2),cell(18,11,2),cell(18,12,2),cell(18,17,1),cell(18,20,1),cell(18,21,2),cell(18,24,2),cell(18,25,2),cell(19,2,2),cell(19,3,3),cell(19,6,1),cell(19,8,1),cell(19,9,1),cell(19,10,1),cell(19,13,1),cell(19,14,2),cell(19,16,1),cell(19,17,3),cell(19,19,3),cell(19,21,3),cell(20,3,1),cell(20,4,1),cell(20,5,2),cell(20,6,3),cell(20,12,2),cell(20,13,3),cell(20,14,2),cell(20,17,2),cell(20,18,2),cell(20,19,2),cell(20,22,1),cell(20,23,3),cell(21,2,3),cell(21,8,2),cell(21,10,2),cell(21,12,1),cell(21,13,1),cell(21,14,2),cell(21,17,1),cell(21,18,3),cell(21,19,2),cell(21,21,2),cell(21,22,3),cell(21,23,2),cell(22,5,2),cell(22,6,3),cell(22,7,2),cell(22,9,2),cell(22,10,2),cell(22,16,3),cell(22,18,3),cell(22,20,2),cell(22,21,1),cell(22,24,1),cell(22,25,2),cell(23,3,2),cell(23,4,1),cell(23,7,3),cell(23,9,2),cell(23,13,1),cell(23,14,3),cell(23,21,3),cell(24,1,3),cell(24,4,2),cell(24,6,1),cell(24,9,2),cell(24,12,3),cell(24,13,1),cell(24,16,3),cell(24,17,3),cell(24,19,1),cell(24,25,3),cell(25,1,2),cell(25,3,2),cell(25,6,3),cell(25,8,2),cell(25,9,2),cell(25,12,2),cell(25,13,3),cell(25,17,0),cell(25,20,3),cell(25,21,2),cell(25,22,2),cell(25,23,2),cell(25,24,0),cell(26,3,2),cell(26,4,3),cell(26,6,2),cell(26,8,1),cell(26,16,1),cell(26,20,2),cell(26,21,1),cell(26,25,3),cell(27,1,1),cell(27,2,2),cell(27,4,2),cell(27,5,3),cell(27,7,3),cell(27,8,1),cell(27,9,3),cell(27,10,0),cell(27,11,3),cell(27,14,2),cell(27,17,2),cell(27,19,3),cell(27,24,1),cell(28,5,2),cell(28,6,0),cell(28,7,2),cell(28,9,3),cell(28,13,2),cell(28,15,1),cell(28,16,2),cell(28,17,2),cell(28,21,2),cell(28,22,2),cell(28,23,3),cell(29,1,1),cell(29,7,2),cell(29,10,2),cell(29,17,2),cell(29,19,3),cell(29,20,3),cell(29,22,1),cell(29,24,3),cell(29,25,3),cell(30,1,3),cell(30,3,1),cell(30,4,2),cell(30,5,1),cell(30,7,2),cell(30,10,2),cell(30,11,2),cell(30,12,1),cell(30,13,2),cell(30,22,3)]).
puzzle(big2,30,25,[cell(1,2,2),cell(1,7,2),cell(1,9,3),cell(1,12,2),cell(1,14,3),cell(1,21,3),cell(1,22,2),cell(1,23,2),cell(1,24,2),cell(2,1,3),cell(2,5,3),cell(2,7,3),cell(2,10,2),cell(2,12,2),cell(2,16,2),cell(2,18,1),cell(2,19,2),cell(2,21,3),cell(2,23,2),cell(2,24,2),cell(2,25,2),cell(3,1,3),cell(3,6,2),cell(3,8,2),cell(3,10,3),cell(3,12,1),cell(3,13,1),cell(3,16,1),cell(3,17,1),cell(3,19,3),cell(3,21,3),cell(3,22,2),cell(4,2,2),cell(4,3,1),cell(4,4,2),cell(4,11,3),cell(4,14,2),cell(4,15,3),cell(4,18,3),cell(4,19,0),cell(4,20,1),cell(4,21,2),cell(4,24,3),cell(5,3,2),cell(5,5,2),cell(5,6,1),cell(5,10,1),cell(5,12,0),cell(5,13,2),cell(5,15,3),cell(5,16,2),cell(5,23,2),cell(6,2,2),cell(6,3,1),cell(6,5,3),cell(6,8,2),cell(6,11,3),cell(6,18,1),cell(6,24,3),cell(7,2,1),cell(7,3,3),cell(7,6,2),cell(7,9,3),cell(7,10,3),cell(7,12,2),cell(7,17,2),cell(7,19,3),cell(7,20,2),cell(7,21,2),cell(7,23,1),cell(8,2,3),cell(8,4,3),cell(8,7,2),cell(8,10,1),cell(8,16,2),cell(8,17,1),cell(8,18,2),cell(8,22,3),cell(8,24,3),cell(9,6,3),cell(9,9,2),cell(9,10,2),cell(9,11,1),cell(9,13,2),cell(9,17,1),cell(9,25,1),cell(10,1,2),cell(10,9,2),cell(10,13,1),cell(10,19,3),cell(10,21,3),cell(10,22,3),cell(10,24,2),cell(11,1,0),cell(11,3,1),cell(11,4,1),cell(11,6,3),cell(11,8,1),cell(11,10,2),cell(11,11,3),cell(11,13,3),cell(11,16,2),cell(11,17,1),cell(11,19,2),cell(11,23,3),cell(12,2,2),cell(12,3,2),cell(12,6,1),cell(12,9,2),cell(12,10,2),cell(12,12,1),cell(12,14,2),cell(12,16,3),cell(12,17,3),cell(12,19,1),cell(12,20,3),cell(12,24,2),cell(12,25,2),cell(13,2,2),cell(13,4,2),cell(13,8,1),cell(13,9,2),cell(13,11,2),cell(13,12,1),cell(13,14,3),cell(13,20,1),cell(13,21,2),cell(13,22,1),cell(14,2,3),cell(14,8,2),cell(14,10,2),cell(14,11,3),cell(14,15,1),cell(14,18,2),cell(14,19,3),cell(14,22,3),cell(14,24,3),cell(15,2,2),cell(15,3,2),cell(15,4,0),cell(15,5,2),cell(15,7,2),cell(15,13,2),cell(15,17,2),cell(15,18,1),cell(15,20,2),cell(15,21,2),cell(15,24,1),cell(16,1,2),cell(16,2,3),cell(16,4,3),cell(16,8,2),cell(16,10,2),cell(16,11,1),cell(16,12,3),cell(16,18,3),cell(16,20,3),cell(16,21,2),cell(16,22,2),cell(16,25,2),cell(17,2,3),cell(17,5,2),cell(17,13,2),cell(17,15,1),cell(17,17,2),cell(17,19,1),cell(17,20,2),cell(17,24,2),cell(17,25,1),cell(18,3,1),cell(18,4,2),cell(18,6,2),cell(18,7,3),cell(18,9,3),cell(18,10,3),cell(18,11,3),cell(18,12,2),cell(18,15,2),cell(18,16,2),cell(18,17,3),cell(18,18,2),cell(18,20,1),cell(18,22,0),cell(18,25,3),cell(19,3,3),cell(19,6,2),cell(19,7,1),cell(19,9,2),cell(19,11,2),cell(19,15,3),cell(19,16,2),cell(19,17,2),cell(19,22,2),cell(19,23,3),cell(19,25,1),cell(20,2,1),cell(20,3,2),cell(20,7,1),cell(20,8,1),cell(20,11,2),cell(20,13,2),cell(20,14,1),cell(20,15,2),cell(20,16,1),cell(20,17,1),cell(20,18,3),cell(20,19,2),cell(20,22,1),cell(20,23,2),cell(20,24,2),cell(21,1,3),cell(21,3,3),cell(21,8,2),cell(21,12,3),cell(21,16,2),cell(21,17,2),cell(21,22,2),cell(22,2,1),cell(22,4,3),cell(22,7,2),cell(22,8,1),cell(22,9,1),cell(22,12,2),cell(22,13,2),cell(22,14,3),cell(22,15,3),cell(22,17,1),cell(22,19,3),cell(22,21,3),cell(22,22,1),cell(22,24,2),cell(23,3,1),cell(23,6,2),cell(23,8,2),cell(23,9,1),cell(23,11,2),cell(23,18,3),cell(23,19,2),cell(23,21,2),cell(23,24,1),cell(23,25,3),cell(24,5,2),cell(24,6,3),cell(24,7,2),cell(24,11,1),cell(24,12,1),cell(24,13,2),cell(24,21,2),cell(24,23,3),cell(24,24,1),cell(25,2,1),cell(25,3,2),cell(25,4,1),cell(25,7,2),cell(25,9,1),cell(25,11,3),cell(25,12,2),cell(25,14,2),cell(25,16,2),cell(25,19,2),cell(25,21,1),cell(25,22,3),cell(25,24,2),cell(26,5,1),cell(26,6,2),cell(26,9,2),cell(26,12,1),cell(26,15,3),cell(26,18,1),cell(26,19,2),cell(26,21,2),cell(27,1,2),cell(27,2,2),cell(27,9,3),cell(27,10,2),cell(27,11,2),cell(27,12,3),cell(27,14,3),cell(27,18,3),cell(27,19,3),cell(27,20,1),cell(28,2,2),cell(28,3,1),cell(28,4,1),cell(28,5,1),cell(28,6,3),cell(28,9,2),cell(28,13,1),cell(28,20,2),cell(28,21,2),cell(28,23,2),cell(28,25,2),cell(29,1,2),cell(29,3,3),cell(29,4,2),cell(29,5,3),cell(29,6,2),cell(29,7,1),cell(29,8,2),cell(29,9,2),cell(29,11,2),cell(29,12,2),cell(29,14,2),cell(29,17,2),cell(29,18,2),cell(29,19,3),cell(29,20,1),cell(29,23,3),cell(29,25,3),cell(30,1,3),cell(30,2,2),cell(30,3,2),cell(30,4,1),cell(30,5,2),cell(30,7,3),cell(30,8,2),cell(30,13,3),cell(30,15,3),cell(30,16,1),cell(30,19,2),cell(30,20,2),cell(30,21,2),cell(30,22,1),cell(30,24,2)]).

puzzle(w,40,30,[cell(1,2,2),cell(1,4,2),cell(1,6,3),cell(1,8,2),cell(1,9,1),cell(1,11,3),cell(1,12,3),cell(1,13,3),cell(1,18,3),cell(1,19,2),cell(1,20,0),cell(1,23,2),cell(1,24,0),cell(1,27,2),cell(1,29,3),cell(2,3,2),cell(2,5,1),cell(2,6,2),cell(2,7,0),cell(2,8,3),cell(2,11,2),cell(2,14,1),cell(2,15,3),cell(2,16,2),cell(2,20,3),cell(2,22,1),cell(2,24,3),cell(2,25,2),cell(2,26,2),cell(2,29,2),cell(3,1,2),cell(3,3,1),cell(3,4,1),cell(3,7,3),cell(3,11,2),cell(3,12,3),cell(3,13,2),cell(3,16,1),cell(3,17,2),cell(3,23,1),cell(3,24,2),cell(3,28,2),cell(3,29,3),cell(4,1,2),cell(4,4,2),cell(4,5,3),cell(4,6,0),cell(4,12,3),cell(4,14,2),cell(4,18,2),cell(4,19,2),cell(4,26,2),cell(4,28,2),cell(4,29,2),cell(4,30,2),cell(5,1,2),cell(5,9,2),cell(5,11,2),cell(5,12,3),cell(5,13,2),cell(5,14,2),cell(5,20,2),cell(5,21,2),cell(5,25,2),cell(5,27,2),cell(5,30,2),cell(6,1,3),cell(6,6,2),cell(6,7,3),cell(6,8,2),cell(6,11,1),cell(6,13,1),cell(6,16,2),cell(6,17,1),cell(6,21,3),cell(6,22,3),cell(6,24,2),cell(6,26,1),cell(6,27,2),cell(6,29,2),cell(6,30,3),cell(7,1,2),cell(7,3,3),cell(7,4,1),cell(7,5,2),cell(7,6,2),cell(7,7,2),cell(7,8,1),cell(7,9,1),cell(7,10,3),cell(7,12,3),cell(7,13,1),cell(7,14,1),cell(7,22,1),cell(7,30,2),cell(8,1,0),cell(8,2,2),cell(8,3,2),cell(8,4,0),cell(8,7,3),cell(8,9,2),cell(8,12,2),cell(8,14,2),cell(8,16,2),cell(8,19,3),cell(8,22,3),cell(8,24,2),cell(8,25,2),cell(8,26,1),cell(8,27,3),cell(8,28,2),cell(8,30,2),cell(9,1,2),cell(9,9,3),cell(9,10,2),cell(9,11,3),cell(9,14,3),cell(9,15,2),cell(9,16,3),cell(9,17,2),cell(9,18,2),cell(9,22,0),cell(9,24,2),cell(9,26,2),cell(9,30,3),cell(10,1,3),cell(10,2,1),cell(10,4,1),cell(10,5,3),cell(10,8,2),cell(10,10,2),cell(10,11,2),cell(10,12,3),cell(10,16,2),cell(10,19,2),cell(10,20,2),cell(10,22,3),cell(10,25,1),cell(10,26,2),cell(10,27,2),cell(10,28,2),cell(10,29,2),cell(10,30,1),cell(11,1,2),cell(11,3,1),cell(11,12,2),cell(11,13,2),cell(11,14,3),cell(11,15,2),cell(11,18,3),cell(11,19,2),cell(11,22,1),cell(11,24,3),cell(11,25,2),cell(11,26,2),cell(11,28,3),cell(11,29,3),cell(12,7,0),cell(12,9,1),cell(12,10,1),cell(12,12,2),cell(12,13,2),cell(12,15,3),cell(12,16,1),cell(12,17,2),cell(12,18,1),cell(12,21,1),cell(12,22,1),cell(12,24,1),cell(12,25,1),cell(12,27,1),cell(13,1,3),cell(13,2,2),cell(13,3,1),cell(13,6,2),cell(13,9,2),cell(13,10,0),cell(13,11,1),cell(13,18,2),cell(13,21,3),cell(13,22,1),cell(13,27,3),cell(13,28,2),cell(13,30,2),cell(14,1,2),cell(14,3,2),cell(14,6,1),cell(14,7,2),cell(14,9,2),cell(14,11,2),cell(14,13,3),cell(14,14,2),cell(14,15,3),cell(14,18,3),cell(14,22,2),cell(14,27,3),cell(14,28,0),cell(14,29,2),cell(14,30,3),cell(15,2,2),cell(15,3,1),cell(15,4,2),cell(15,6,2),cell(15,9,2),cell(15,11,3),cell(15,12,1),cell(15,13,2),cell(15,14,0),cell(15,15,3),cell(15,17,1),cell(15,18,2),cell(15,20,2),cell(15,21,1),cell(15,22,2),cell(15,26,2),cell(15,28,2),cell(15,30,2),cell(16,1,2),cell(16,4,1),cell(16,5,2),cell(16,6,2),cell(16,10,2),cell(16,12,2),cell(16,18,2),cell(16,20,2),cell(16,21,2),cell(16,22,2),cell(16,24,1),cell(16,28,0),cell(16,29,2),cell(16,30,3),cell(17,4,2),cell(17,6,2),cell(17,7,0),cell(17,8,2),cell(17,9,2),cell(17,10,3),cell(17,14,1),cell(17,15,1),cell(17,18,2),cell(17,20,3),cell(17,23,2),cell(17,24,3),cell(17,25,2),cell(17,28,2),cell(17,29,3),cell(18,1,2),cell(18,4,1),cell(18,5,3),cell(18,6,3),cell(18,7,2),cell(18,10,1),cell(18,15,3),cell(18,18,2),cell(18,19,1),cell(18,20,2),cell(18,21,1),cell(18,22,2),cell(18,27,2),cell(18,28,2),cell(18,29,1),cell(18,30,3),cell(19,1,2),cell(19,4,2),cell(19,7,2),cell(19,9,3),cell(19,10,3),cell(19,13,3),cell(19,16,2),cell(19,17,3),cell(19,18,2),cell(19,25,2),cell(19,27,2),cell(19,28,3),cell(19,30,3),cell(20,1,3),cell(20,2,1),cell(20,3,2),cell(20,5,2),cell(20,6,2),cell(20,7,2),cell(20,9,2),cell(20,10,2),cell(20,13,2),cell(20,15,1),cell(20,20,2),cell(20,21,2),cell(20,22,3),cell(20,25,3),cell(20,28,2),cell(20,29,1),cell(21,1,2),cell(21,2,1),cell(21,8,2),cell(21,9,2),cell(21,10,2),cell(21,12,1),cell(21,13,1),cell(21,14,2),cell(21,17,3),cell(21,18,1),cell(21,20,2),cell(21,25,1),cell(21,26,3),cell(21,29,3),cell(22,2,2),cell(22,4,1),cell(22,6,2),cell(22,7,1),cell(22,12,2),cell(22,14,2),cell(22,15,1),cell(22,16,1),cell(22,17,3),cell(22,18,1),cell(22,23,2),cell(22,24,3),cell(22,25,2),cell(22,26,2),cell(22,28,3),cell(22,29,1),cell(23,1,3),cell(23,2,2),cell(23,5,1),cell(23,6,2),cell(23,8,2),cell(23,12,1),cell(23,13,1),cell(23,14,3),cell(23,15,2),cell(23,16,3),cell(23,18,1),cell(23,22,3),cell(23,23,2),cell(23,24,2),cell(23,25,1),cell(23,26,3),cell(23,27,1),cell(23,28,3),cell(23,29,2),cell(24,1,2),cell(24,5,3),cell(24,7,2),cell(24,8,2),cell(24,9,3),cell(24,10,1),cell(24,15,1),cell(24,16,1),cell(24,17,3),cell(24,18,1),cell(24,20,1),cell(24,24,3),cell(24,25,2),cell(24,27,1),cell(24,28,3),cell(25,2,2),cell(25,3,2),cell(25,6,0),cell(25,7,3),cell(25,9,2),cell(25,11,2),cell(25,13,1),cell(25,15,3),cell(25,16,2),cell(25,17,3),cell(25,21,2),cell(25,23,2),cell(25,25,1),cell(25,26,2),cell(25,27,1),cell(25,28,1),cell(25,29,3),cell(26,7,3),cell(26,9,2),cell(26,12,2),cell(26,14,3),cell(26,15,1),cell(26,16,1),cell(26,19,2),cell(26,20,3),cell(26,21,2),cell(26,22,2),cell(26,23,3),cell(26,26,2),cell(26,28,2),cell(26,29,2),cell(26,30,2),cell(27,1,2),cell(27,2,1),cell(27,3,2),cell(27,4,2),cell(27,5,3),cell(27,6,1),cell(27,7,1),cell(27,11,1),cell(27,12,1),cell(27,13,0),cell(27,14,2),cell(27,15,2),cell(27,16,2),cell(27,18,1),cell(27,19,2),cell(27,20,2),cell(27,22,2),cell(27,30,2),cell(28,3,2),cell(28,4,0),cell(28,5,2),cell(28,7,2),cell(28,9,2),cell(28,16,2),cell(28,17,3),cell(28,18,2),cell(28,22,2),cell(28,24,3),cell(28,25,2),cell(28,26,3),cell(28,28,3),cell(28,30,2),cell(29,1,2),cell(29,3,1),cell(29,7,2),cell(29,10,1),cell(29,11,1),cell(29,12,2),cell(29,14,2),cell(29,16,2),cell(29,17,2),cell(29,21,1),cell(29,22,3),cell(29,23,2),cell(29,24,2),cell(29,26,2),cell(29,28,2),cell(29,30,3),cell(30,1,3),cell(30,8,3),cell(30,9,3),cell(30,10,2),cell(30,11,1),cell(30,12,2),cell(30,14,2),cell(30,15,1),cell(30,21,3),cell(30,28,1),cell(31,1,3),cell(31,6,2),cell(31,7,1),cell(31,11,3),cell(31,15,1),cell(31,16,3),cell(31,18,2),cell(31,19,2),cell(31,20,0),cell(31,21,3),cell(31,24,2),cell(31,25,1),cell(31,27,1),cell(31,29,3),cell(31,30,2),cell(32,1,2),cell(32,7,2),cell(32,8,3),cell(32,10,2),cell(32,11,1),cell(32,16,2),cell(32,19,3),cell(32,20,3),cell(32,23,2),cell(32,24,3),cell(32,27,1),cell(32,30,2),cell(33,2,3),cell(33,3,2),cell(33,4,3),cell(33,7,2),cell(33,8,0),cell(33,9,1),cell(33,10,1),cell(33,12,3),cell(33,14,3),cell(33,15,2),cell(33,16,1),cell(33,17,2),cell(33,23,2),cell(33,24,2),cell(33,25,1),cell(33,26,2),cell(33,28,2),cell(33,29,2),cell(34,2,1),cell(34,3,2),cell(34,4,0),cell(34,8,1),cell(34,9,2),cell(34,10,3),cell(34,11,1),cell(34,12,2),cell(34,15,2),cell(34,17,3),cell(34,18,1),cell(34,19,2),cell(34,20,3),cell(34,24,2),cell(34,28,1),cell(35,1,3),cell(35,2,2),cell(35,5,3),cell(35,8,1),cell(35,11,2),cell(35,12,0),cell(35,13,2),cell(35,15,3),cell(35,16,1),cell(35,17,2),cell(35,18,2),cell(35,20,2),cell(35,21,1),cell(35,23,2),cell(35,26,2),cell(35,27,1),cell(35,29,3),cell(36,1,2),cell(36,3,2),cell(36,4,3),cell(36,5,1),cell(36,7,1),cell(36,8,0),cell(36,10,2),cell(36,13,2),cell(36,17,3),cell(36,18,0),cell(36,19,2),cell(36,20,3),cell(36,21,1),cell(36,26,2),cell(36,27,1),cell(36,28,1),cell(36,29,3),cell(37,1,2),cell(37,3,0),cell(37,4,2),cell(37,5,1),cell(37,13,2),cell(37,17,2),cell(37,20,2),cell(37,24,1),cell(37,25,2),cell(37,27,2),cell(37,29,2),cell(38,1,1),cell(38,2,3),cell(38,9,3),cell(38,10,2),cell(38,19,2),cell(38,23,2),cell(38,24,0),cell(38,26,2),cell(38,27,2),cell(39,1,2),cell(39,2,2),cell(39,3,2),cell(39,4,2),cell(39,5,2),cell(39,7,1),cell(39,8,2),cell(39,11,2),cell(39,12,1),cell(39,13,2),cell(39,14,0),cell(39,15,2),cell(39,16,1),cell(39,19,2),cell(39,20,2),cell(39,21,3),cell(39,22,2),cell(39,23,3),cell(39,25,3),cell(39,26,0),cell(39,27,2),cell(39,28,1),cell(39,30,2),cell(40,4,2),cell(40,5,2),cell(40,8,2),cell(40,9,2),cell(40,13,3),cell(40,14,3),cell(40,20,1),cell(40,23,2),cell(40,24,1),cell(40,26,3),cell(40,28,3),cell(40,29,3)]).

