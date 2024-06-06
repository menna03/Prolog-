:-consult(data).


% Question 1

custom_member(X, [X|L]):- !.
custom_member(X, [_|L]):- 
	custom_member(X, L).

list_orders(CustomerName, Orders) :-
    
    list_helper(CustomerID, Orders).

list_helper(CustomerID, Orders) :-
    list_helper(CustomerID, [], Orders).

list_helper(CustomerID, Acc, Orders) :-
    order(CustomerID, OrderID, Items),
    \+custom_member(order(CustomerID, OrderID, Items), Acc),  %\+ ma3naha not member
    list_helper(CustomerID, [order(CustomerID, OrderID, Items)|Acc], Orders).
list_helper(_, Orders, Orders).  


% Question 2

custom_length([], 0). % Base case

custom_length([_|Tail], Len) :-
    custom_length(Tail, LenTail), 
    Len is LenTail + 1.

countOrdersOfCustomer(CustomerName, Count) :-
    list_orders(CustomerName, Orders),
    custom_length(Orders, Count).

% Question 3

getItemsInOrderById(CustomerName, OrderID, Items) :-
    customer(CustomerID, CustomerName),
    order(CustomerID, OrderID, Items). 


% Question 4

getNumOfItems(CustomerName, OrderId, Count) :-
    customer(CustomerId, CustomerName),
    order(CustomerId, OrderId, Items),
    custom_length(Items, Count).


% Question 5

calcPriceOfOrder(CustomerName, OrderID, TotalPrice) :-
    customer(CustomerID, CustomerName),
    order(CustomerID, OrderID, Items),
    calcOrderPrice(Items, TotalPrice, 0).

calcOrderPrice([], TotalPrice, TotalPrice).
calcOrderPrice([Item|Rest], TotalPrice, Acc) :-
    item(Item, _, Price),
    NewAcc is Acc + Price,
    calcOrderPrice(Rest, TotalPrice, NewAcc).


% Question 6

isBoycott(Item) :-
    boycott_company(Company, _),
    item(Item, Company, _).



% Question 7

whyToBoycott(Item, Justification) :-
    boycott_company(Company, Justification),
    item(Item, Company, _).


% Question 8

removeBoycottItemsFromAnOrder(CustomerUsername, OrderID, NewList) :-
    customer(CustomerID, CustomerUsername),
    order(CustomerID, OrderID, Items),
    removeBoycottItems(Items, NewList).

removeBoycottItems([], []).
removeBoycottItems([Item|Rest], NewList) :-
    isBoycott(Item),
    !,                                 % Cut to stop further backtracking
    removeBoycottItems(Rest, NewList).

removeBoycottItems([Item|Rest], [Item|NewRest]) :-
    removeBoycottItems(Rest, NewRest).

% Question 9

custom_suffix(Suffixed, List):-
    custom_append(_, Suffixed, List).

custom_append([], L, L).
custom_append([H|T], L2, [H|NT]):-
    custom_append(T, L2, NT).

replaceBoycottItemsFromAnOrder(CustomerUsername, OrderID, NewList) :-
    customer(CustomerID, CustomerUsername),
    order(CustomerID, OrderID, Items),
    replaceBoycottItems(Items, NewList, []).

replaceBoycottItems([], Acc, Acc).
replaceBoycottItems([Item|Rest], NewList, Acc) :-
    (isBoycott(Item) ->
        alternative(Item, AltItem),
        custom_append(Acc, [AltItem], NewAcc)
    ;
        custom_append(Acc, [Item], NewAcc)
    ),
    replaceBoycottItems(Rest, NewList, NewAcc).


% Question 10

calcPriceAfterReplacingBoycottItemsFromAnOrder(CustomerUsername, OrderID, NewList, TotalPrice) :-
    replaceBoycottItemsFromAnOrder(CustomerUsername, OrderID, NewList),
    calcPriceOfItems(NewList, TotalPrice).

calcPriceOfItems([], 0).
calcPriceOfItems([Item|Rest], TotalPrice) :-
    item(Item, _, Price),
    calcPriceOfItems(Rest, RestPrice),
    TotalPrice is Price + RestPrice.

% Question 11

getTheDifferenceInPriceBetweenItemAndAlternative(Item, Alternative, DiffPrice) :-
    alternative(Item, Alternative),
    item(Item, _, ItemPrice),
    item(Alternative, _, AltPrice),
    DiffPrice is ItemPrice - AltPrice .

% Queston 12

:- dynamic item/3.
:- dynamic alternative/2.
:- dynamic boycott/1.

add_item(ItemID, Name, Quantity) :-
    assert(item(ItemID, Name, Quantity)).

remove_item(ItemID, Name, Quantity) :-
    retract(item(ItemID, Name, Quantity)).

add_alternative(ItemID, AltName) :-
    assert(alternative(ItemID, AltName)).

remove_alternative(ItemID, AltName) :-
    retract(alternative(ItemID, AltName)).

add_boycott(Company) :-
    assert(boycott(Company)).

remove_boycott(Company) :-
    retract(boycott(Company)).

