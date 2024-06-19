% Define moves to get next state
move(State, NextState):-
    move_helper(State, NextState, 1, 1). %helper predicate moves acros rows and columns for board

move_helper([Row|Rest], [NewRow|Rest], R, 1):-
    R1 is R + 1,
    move_helper(Row, NewRow, R1, 1). % row

move_helper([Cell|Row], [Cell|NewRow], R, C):-
    C1 is C + 1,   % columns
    move_helper(Row, NewRow, R, C1).

move_helper([], [], _, _). % Base case Empity 

print_board([]). % print the board 
print_board([Row|Rest]):-
    writeln(Row),
    print_board(Rest).

% Predicate to check if a state is not already in the open or closed list
not_member(_, []). % These cells are different (no duplicates)
not_member(State, [OtherState|Rest]):-
    State \= OtherState,
    not_member(State, Rest).

% Get next state based on current state, open and closed lists
get_next_state([State, _], Open, Closed, [NextState, State]):-
    move(State, NextState),
    not_member([NextState, _], Open),
    not_member([NextState, _], Closed),
    NextState \= State.  % Ensure Next state is different from the current state

% Get all valid children states
get_all_valid_children(Node, Open, Closed, Children):-
    findall(NextState, (get_next_state(Node, Open, Closed, NextState), 
    writeln('Generated Next State:'), writeln(NextState)), Children).

% Breadth-first search algorithm
breadth_first_search(Open, Closed, Goal):-    % if solution found
    get_state(Open, [CurrentState,Parent], _),
    CurrentState = Goal, !, %  search is complete  
    write("Search is complete!"), nl,
    print_solution([CurrentState,Parent], Closed).

breadth_first_search(Open, Closed, Goal):- % if not found 
    get_state(Open, CurrentNode, TmpOpen),
    get_all_valid_children(CurrentNode,TmpOpen,Closed,Children),
    add_children(Children, TmpOpen, NewOpen), % valid children states
    append(Closed, [CurrentNode], NewClosed),
    breadth_first_search(NewOpen, NewClosed, Goal).

% Get state from open list
get_state([CurrentNode|Rest], CurrentNode, Rest).

% Add children to open list
add_children(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).

% Print solution path
print_solution([State, null],_):- % Base Case
    write(State), nl.

print_solution([State, Parent], Closed):-
    member([Parent, GrandParent], Closed),
    print_solution([Parent, GrandParent], Closed),
    write(State), nl.

% Define input
% Example board
initial_board([
    [yellow, yellow, red, red],
    [blue, yellow, blue, red],
    [blue, blue, blue, yellow],
    [blue, blue, blue, yellow]
]).

% Predicate to get the color of a cell at specified coordinates
get_cell_color(Board, X, Y, Color) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Color).

% Predicate to check if a cell is within the bounds of the board
cell_within_bounds(Board, X, Y) :-
    length(Board, N),
    X >= 0, X < N,
    nth0(X, Board, Row),
    length(Row, M),
    Y >= 0, Y < M.

% Predicate to get the adjacent cells of the same color
adjacent_same_color(Board, X, Y, AdjX, AdjY, Color) :-
    Dx is X - 1, Dy is Y,                      % Move left
    cell_within_bounds(Board, Dx, Dy),
    get_cell_color(Board, Dx, Dy, Color),
    AdjX = Dx, AdjY = Dy.

adjacent_same_color(Board, X, Y, AdjX, AdjY, Color) :-
    Dx is X + 1, Dy is Y,                      % Move right
    cell_within_bounds(Board, Dx, Dy),
    get_cell_color(Board, Dx, Dy, Color),
    AdjX = Dx, AdjY = Dy.

adjacent_same_color(Board, X, Y, AdjX, AdjY, Color) :-
    Dx is X, Dy is Y - 1,                       % Move up
    cell_within_bounds(Board, Dx, Dy),
    get_cell_color(Board, Dx, Dy, Color),
    AdjX = Dx, AdjY = Dy.

adjacent_same_color(Board, X, Y, AdjX, AdjY, Color) :-
    Dx is X, Dy is Y + 1,                      % Move down
    cell_within_bounds(Board, Dx, Dy),
    get_cell_color(Board, Dx, Dy, Color),
    AdjX = Dx, AdjY = Dy.

% Define a predicate to find a cycle starting from a given cell
find_cycle(Board, X, Y, Visited, Cycle, Color) :-
    length(Visited, L),
    L >= 4, %       At least 4 cells in the cycle
    member([X, Y], Visited), % checkk iif we reached the starting cell again
    get_cell_color(Board, X, Y, Color), % Ensure it's the same color as the starting cell
    Cycle = Visited. % Found a cycle

find_cycle(Board, X, Y, Visited, Cycle, Color) :-
    get_cell_color(Board, X, Y, Color),
    adjacent_same_color(Board, X, Y, AdjX, AdjY, Color),
    \+ member([AdjX, AdjY], Visited),       % Ensure we haven"t visit this cell before
    find_cycle(Board, AdjX, AdjY, [[AdjX, AdjY]|Visited], Cycle, Color).

% Define a predicate to start the search for cycles
find_cycles(Board) :-
    length(Board, N),
    between(0, N, X),
    between(0, N, Y),
    get_cell_color(Board, X, Y, Color), % ensure the cell isnt empty
    find_cycle(Board, X, Y, [], Cycle, Color),
    length(Cycle, L),
    L >= 4, % Ensure the cycle has at least 4 cells
    write('Cycle found with color '), write(Color), write(': '), write(Cycle), nl.

% Define a predicate to solve the puzzle
searchCycles :-
    initial_board(Board),
    find_cycles(Board),
    !. % Cut to stop searching after finding the first cycle

% If no cycles are found, print a message
searchCycles:-
    write('No cycles found.'), nl.