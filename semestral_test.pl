:- load_files(semestral).
:- use_module(library(plunit)).
:- begin_tests(semestral_test).

%search([H|_], cell(X,0), Res)
test(search) :-
    search([[1,1],[1,1]], cell(0,0), 1),
    search([[0,1],[1,1]], cell(0,0), 0),
    search([[1,5],[1,1]], cell(1,0), 5),
    search([[1,1],[1,9]], cell(1,1), 9),
    search([[0,0],[9,1]], cell(0,1), 9),
    search([[0,0,0],[0,7,0],[0,0,0]], cell(1,1), 7).


%top(cell(X, Y), World, Res)
test(directions) :-
    top(cell(0,1), [[1,2],[3,4]], 1),
    bottom(cell(0,0), [[1,2],[3,4]], 3),
    right(cell(0,0), [[1,2],[3,4]], 2),
    left(cell(1,0), [[1,2],[3,4]], 1),
    top_right(cell(0,1), [[1,2],[3,4]], 2). 


%neighbours(cell(X,Y), World, Count)
test(neighbours) :-
    neighbours(cell(1,1), [[0,0,0],[0,0,0],[0,0,0]], 0),
    neighbours(cell(1,1), [[0,0,0],[0,1,0],[0,0,0]], 0),
    neighbours(cell(1,1), [[1,0,0],[0,1,0],[0,0,0]], 1),
    neighbours(cell(1,1), [[1,0,0],[0,1,0],[1,0,0]], 2),
    neighbours(cell(1,1), [[1,1,1],[1,1,1],[1,1,1]], 8),
    neighbours(cell(1,1), [[1,1,1],[1,0,1],[1,1,1]], 8),
    neighbours(cell(0,0), [[1,1,1],[1,1,1],[1,1,1]], 3).

%evaluate_cell(Cell, New_cell, X, Y, World)
test(cell_evaluation) :-
    evaluate_cell(0, 0, 0, 0, [[0,0],[0,0]]),
    evaluate_cell(1, 0, 0, 0, [[1,0],[0,0]]),
    evaluate_cell(1, 1, 0, 0, [[1,1],[1,0]]),
    evaluate_cell(1, 1, 0, 0, [[1,1],[0,1]]),
    evaluate_cell(1, 1, 0, 0, [[1,1],[1,1]]),
    evaluate_cell(1, 0, 1, 1, [[1,1,1],[1,1,1],[1,1,1]]),
    evaluate_cell(1, 0, 1, 1, [[1,0,0],[0,1,0],[0,1,0]]),
    evaluate_cell(1, 0, 1, 1, [[0,0,0],[0,1,0],[0,0,0]]).

lists_are_equal([],[]) :- !.
lists_are_equal([Row|Rows], [Row2|Rows2]) :-
    rows_are_equal(Row, Row2),
    lists_are_equal(Rows,Rows2).

rows_are_equal([], []).
rows_are_equal([H1|R1], [H2|R2]):-
    H1 = H2,
    rows_are_equal(R1, R2).

%evaluate_rows([Row|Rows],[New_row|New_rows], Y, World)
test(game_of_life) :-
    evaluate_rows([[1,1],[1,1]], New, 0, [[1,1],[1,1]]),
    lists_are_equal(New, [[1,1],[1,1]]).

test(game_of_life1) :-
    evaluate_rows([[1,0],[0,1]], New, 0, [[1,0],[0,1]]),
    lists_are_equal(New, [[0,0],[0,0]]).

test(game_of_life2) :-
    evaluate_rows([[0,0],[0,0]], New, 0, [[0,0],[0,0]]),
    lists_are_equal(New, [[0,0],[0,0]]).

test(game_of_life3) :-
    evaluate_rows([[1,0],[1,1]], New, 0, [[1,0],[1,1]]),
    lists_are_equal(New, [[1,1],[1,1]]).

test(game_of_life4) :-
    evaluate_rows([[1,1,1],[1,1,1],[1,1,1]], New, 0, [[1,1,1],[1,1,1],[1,1,1]]),
    lists_are_equal(New, [[1,0,1],[0,0,0],[1,0,1]]).

test(game_of_life5) :-
    evaluate_rows([[1,0,1],[0,0,0],[1,0,1]], New, 0, [[1,0,1],[0,0,0],[1,0,1]]),
    lists_are_equal(New, [[0,0,0],[0,0,0],[0,0,0]]).

test(game_of_lif6) :-
    evaluate_rows([[1,0,1],[0,0,0],[1,0,1]], New, 0, [[1,0,1],[0,0,0],[1,0,1]]),
    lists_are_equal(New, [[0,0,0],[0,0,0],[0,0,0]]).

test(game_of_life7) :-
    evaluate_rows([[0,0,0,0,0],[0,0,1,0,0],[0,1,0,1,0],[0,0,1,0,0],[0,0,0,0,0]], New, 0,
    [[0,0,0,0,0],[0,0,1,0,0],[0,1,0,1,0],[0,0,1,0,0],[0,0,0,0,0]]),
    lists_are_equal(New, [[0,0,0,0,0],[0,0,1,0,0],[0,1,0,1,0],[0,0,1,0,0],[0,0,0,0,0]]).


test(game_of_life8) :-
    evaluate_rows([[0,0,0,0,0],[0,1,1,0,0],[0,1,0,1,0],[0,0,1,0,0],[0,0,0,0,0]], New, 0,
        [[0,0,0,0,0],[0,1,1,0,0],[0,1,0,1,0],[0,0,1,0,0],[0,0,0,0,0]]),
    lists_are_equal(New, [[0,0,0,0,0],[0,1,1,0,0],[0,1,0,1,0],[0,0,1,0,0],[0,0,0,0,0]]).

test(game_of_life9) :-
    evaluate_rows([[0,0,0,0,0,0],[0,0,1,0,0,0],[0,0,1,0,0,0],[0,0,1,0,0,0],[0,0,0,0,0,0]], New, 0,
        [[0,0,0,0,0,0],[0,0,1,0,0,0],[0,0,1,0,0,0],[0,0,1,0,0,0],[0,0,0,0,0,0]]),
    lists_are_equal(New, [[0,0,0,0,0,0],[0,0,0,0,0,0],[0,1,1,1,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]]),
    evaluate_rows([[0,0,0,0,0,0],[0,0,0,0,0,0],[0,1,1,1,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]], New2, 0,
        [[0,0,0,0,0,0],[0,0,0,0,0,0],[0,1,1,1,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]]),
    lists_are_equal(New2, [[0,0,0,0,0,0],[0,0,1,0,0,0],[0,0,1,0,0,0],[0,0,1,0,0,0],[0,0,0,0,0,0]]). 


:- end_tests(semestral_test).