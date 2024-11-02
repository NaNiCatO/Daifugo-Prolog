% Sample rank order for cards (simplified)
rank_order([3, 4, 5, 6, 7, 8, 9, 10, j, q, k, a, 2]).

% Check if a single card move is valid
valid_move([card(Rank1, _)], [card(Rank2, _)]) :-
    rank_order(Order),
    nth1(Index1, Order, Rank1),
    nth1(Index2, Order, Rank2),
    Index1 > Index2.

% Check if multiple cards are valid (e.g., pairs or triples)
valid_move(Cards, LastPlayedCards) :-
    % Ensure that all cards in the move have the same rank
    same_rank(Cards),
    same_rank(LastPlayedCards),
    Cards = [card(Rank1, _) | _],
    LastPlayedCards = [card(Rank2, _) | _],
    valid_move([card(Rank1, _)],[card(Rank2, _)]).

% Helper predicate to check that all cards have the same rank
same_rank([card(Rank, _)]).
same_rank([card(Rank, _) | Rest]) :- same_rank(Rest).
