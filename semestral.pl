%find cell with given coords in matrix
search([H|_], cell(X,0), Res) :- 
    x_search(H, cell(X, 0), Res), !.
search([_|T], cell(X, Y), Res) :- Y1 is Y - 1, search(T, cell(X, Y1), Res).

x_search([H|_], cell(0,0), H) :- !.
x_search([_|T], cell(X, 0), Res) :- X1 is X - 1, x_search(T, cell(X1, 0), Res).

%look in each direction from given cell
top(cell(X, Y), World, Res) :- Y1 is Y - 1, search(World, cell(X, Y1), Res).

bottom(cell(X, Y), World, Res) :- Y1 is Y + 1, search(World, cell(X, Y1), Res).

right(cell(X, Y), World, Res) :- X1 is X + 1, search(World, cell(X1, Y), Res).

left(cell(X, Y), World, Res) :- X1 is X - 1, search(World, cell(X1, Y), Res).

top_right(cell(X, Y), World, Res) :- 
    X1 is X + 1,
    Y1 is Y - 1 ,
    search(World, cell(X1, Y1), Res).

top_left(cell(X, Y), World, Res) :- 
    X1 is X - 1,
    Y1 is Y - 1 ,
    search(World, cell(X1, Y1), Res).

bottom_left(cell(X, Y), World, Res) :- 
    X1 is X - 1,
    Y1 is Y + 1 ,
    search(World, cell(X1, Y1), Res).

bottom_right(cell(X, Y), World, Res) :- 
    X1 is X + 1,
    Y1 is Y + 1 ,
    search(World, cell(X1, Y1), Res).

%determine whether cell is dead/alive
is_alive(Cell) :- Cell = 1.
is_dead(Cell) :- Cell = 0.

count_neighbours(cell(X,Y), World) :-  top(cell(X,Y), World, Cell), is_alive(Cell).
count_neighbours(cell(X,Y), World) :-  bottom(cell(X,Y), World, Cell), is_alive(Cell).
count_neighbours(cell(X,Y), World) :-  right(cell(X,Y), World, Cell), is_alive(Cell).
count_neighbours(cell(X,Y), World) :-  left(cell(X,Y), World, Cell), is_alive(Cell).
count_neighbours(cell(X,Y), World) :-  top_left(cell(X,Y), World, Cell), is_alive(Cell).
count_neighbours(cell(X,Y), World) :-  top_right(cell(X,Y), World, Cell), is_alive(Cell).
count_neighbours(cell(X,Y), World) :-  bottom_right(cell(X,Y), World, Cell), is_alive(Cell).
count_neighbours(cell(X,Y), World) :-  bottom_left(cell(X,Y), World, Cell), is_alive(Cell).

%count live neighbours of given cell in each direction.
neighbours(cell(X,Y), World, Count) :- aggregate_all(count, count_neighbours(cell(X,Y), World), Count).

%Any dead cell with three live neighbours becomes a live cell.
evaluate_cell(Cell, New_cell, X, Y, World) :-
    is_dead(Cell),
    neighbours(cell(X, Y), World, Count),
    Count = 3,
    New_cell is 1,
    !.
%Any live cell with two live neighbours survives.
evaluate_cell(Cell, New_cell, X, Y, World) :-
    is_alive(Cell),
    neighbours(cell(X, Y), World, Count),
    Count = 2,
    New_cell is 1,
    !.
%Any live cell with three live neighbours survives.
evaluate_cell(Cell, New_cell, X, Y, World) :-
    is_alive(Cell),
    neighbours(cell(X, Y), World, Count),
    Count = 3,
    New_cell is 1,
    !.
%All other live cells die in the next generation. Similarly, all other dead cells stay dead.
evaluate_cell(_, New_cell, _, _, _) :-
    New_cell is 0.


%evaluate each element in a row
evaluate_row([Cell|Columns],[New_cell|New_columns], X, Y, World) :-
    evaluate_cell(Cell, New_cell, X, Y, World),
    X1 is X + 1,
    evaluate_row(Columns,New_columns, X1, Y, World).
evaluate_row([],[],_,_,_).

%evaluate each row from world
evaluate_rows([Row|Rows],[New_row|New_rows], Y, World) :-
    evaluate_row(Row,New_row, 0, Y, World),
    Y1 is Y + 1,
    evaluate_rows(Rows,New_rows, Y1, World).
evaluate_rows([],[], _, _).

%printing the world
%each live cell is represented by +, dead cell by -
print_row([]) :- !.
print_row([Elem|Row]) :-
    Elem = 1,
    write('+'),
    write(' '),
    print_row(Row).
print_row([_|Row]) :-
    write('-'),
    write(' '),
    print_row(Row).

%after printing each row, print newline
print_rows([]) :- !.
print_rows([Row|Rows]) :-
    print_row(Row),
    write('\n'),
    print_rows(Rows).

main_loop(_, _,0) :- !.
main_loop(World, New_world, Count) :-
    evaluate_rows(World, New_world, 0, World),
    Count2 is Count - 1,
    print_rows(New_world),
    write('\n'),
    sleep(1),
    main_loop(New_world, _, Count2).

main(World, Iteration) :-
    main_loop(World, _, Iteration),
    !.