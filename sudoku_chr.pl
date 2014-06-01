:- module(sudoku_chr, [foo/1]).
:- use_module(library(chr)).
:- chr_option(debug, on). % on - off
:- chr_option(optimize, off). % full - off
:- chr_option(check_guard_bindings, on). % on - off

:- chr_constraint foo/1, bar/2, foo(+,?,-list(int)).
% int, dense_int (can be used as array index), float, number, natural, any
% chr_type list(T) ---> [] ; [T | list(T)].
% chr_type alias == list(list(int)).

