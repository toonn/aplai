% APLAI assignment due 2014-06-04
% via email to Professor G. Janssens
% Task 1: Sudoku
% Task 1.B: Viewpoints and Programs
% System for implementation: CHR

:- use_module(library(chr)).
:- chr_constraint(sudoku/1). % Types and modes?
% Classical Viewpoint
%   all numbers in a row must be different
%   all numbers in a column must be different
%   all numbers in a block must be different



% Alternative Viewpoint
%   every number in {1..9} is associated with a set of cardinality 9
%   these sets are all disjunct
%   each set contains a number from every column
%   each set contains a number from every row
%   each set contains a number from every block
