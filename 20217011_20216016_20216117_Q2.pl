% Define the board
board([
    [red, red, yellow, yellow],
    [red, blue, red, red],
    [red, red, red, yellow],
    [blue, red, blue, yellow]
]).

% Check if a cell is valid on the board
validCell([X,Y], Board) :-
    length(Board, NumRows),
    length(Board, NumCols),
    X >= 0, X < NumRows,
    Y >= 0, Y < NumCols,
    nth0(X, Board, Row),
    nth0(Y, Row, _).

% Predicate to find the path from start to goal
findPath :-
    % Get input board  start  goal  and color
    input(Board, Start, Goal, Color),
    search([[Start, null, 0, 0, 0]], [], Goal, Board, Color).

% input predicate to get the board start  goal  and color from the user
input(Board, Start, Goal, Color) :-
    board(Board),
    write('Enter the start position (e.g., [0,0]): '),
    read(Start),
    write('Enter the goal position (e.g., [2,2]): '),
    read(Goal),
    write('Enter the color (red, yellow, or blue): '),
    read(Color).

% Search algorithm
search(Open, Closed, Goal, Board, Color):-  % base case
    getBestState(Open, [CurrentState, Parent, G, _, _], Rest), 
    CurrentState = Goal, 
    write("Search is complete!"), nl,
    printSolution([CurrentState, Parent, G, _, _], Closed), !.

search(Open, Closed, Goal, Board, Color):-
    getBestState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(CurrentNode, TmpOpen, Closed, Goal, Board, Color, Children), 
    addChildren(Children, TmpOpen, NewOpen),
    append(Closed, [CurrentNode], NewClosed), 
    search(NewOpen, NewClosed, Goal, Board, Color). 

% get the next states
getAllValidChildren(Node, Open, Closed, Goal, Board, Color, Children):-
    findall(Next, getNextState(Node, Open, Closed, Goal, Board, Color, Next), Children).

getNextState([State, _, G, _, _], Open, Closed, Goal, Board, Color, [Next, State, NewG, NewH, NewF]):-
    move(State, Next, MoveCost),
    calculateH(Next, Goal, NewH),
    NewG is G + MoveCost,
    NewF is NewG + NewH,
    (not(member([Next, _, _, _, _], Open)) ; memberButBetter(Next, Open, NewF)),
    (not(member([Next, _, _, _, _], Closed)) ; memberButBetter(Next, Closed, NewF)),
    validCell(Next, Board),
    cellColor(Next, Board, CellColor),
    CellColor = Color.

% Predicate to check if a cell is of the specified color
cellColor([X, Y], Board, Color) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Color).

% Heuristic function to estimate the remaining cost from a cell to the goal cell (always returns 1)
calculateH(_, _, 1).

% Predicate to move from one cell to its adjacent cells
move([X1,Y], [X2,Y], 1) :- X2 is X1 + 1. % Move right

move([X1,Y], [X2,Y], 1) :- X2 is X1 - 1. % Move left

move([X,Y1], [X,Y2], 1) :- Y2 is Y1 + 1. % Move down

move([X,Y1], [X,Y2], 1) :- Y2 is Y1 - 1. % Move up

% Predicate to add children to the open list
addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).

% Predicate to print the path solution
printSolution([State, null, G, H, F],_):- % base case
    writeCoords(State, G), nl.

printSolution([State, Parent, G, H, F], Closed):-
    member([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    printSolution([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    write(' -> '),
    writeCoords(State, G), nl.

% Predicate to write the coordinates of a cell
writeCoords([X,Y], G) :-
    write(X), write(','), write(Y), write(' ('), write(G), write(')').

% Predicate to get the best state from the open list
getBestState([State|RestOpen], BestState, RestOpen):-
    findMin([State|RestOpen], BestState).

findMin([X], X):- !.
findMin([Head|T], Min):-
    findMin(T, TmpMin),
    Head = [_,_,_,HeadH,HeadF],
    TmpMin = [_,_,_,TmpH,TmpF],
    (TmpF < HeadF -> Min = TmpMin ; Min = Head).

% Helper predicate to ensure that a cell is not a member of a list with a better F value
memberButBetter(Next, List, NewF):-
    findall(F, member([Next,_,_,_,F], List), Numbers),
    min_list(Numbers, MinOldF),
    MinOldF > NewF.
