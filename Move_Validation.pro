% Define rank values for comparison purposes
rank_value('2', 2).
rank_value('3', 3).
rank_value('4', 4).
rank_value('5', 5).
rank_value('6', 6).
rank_value('7', 7).
rank_value('8', 8).
rank_value('9', 9).
rank_value('10', 10).
rank_value('J', 11).
rank_value('Q', 12).
rank_value('K', 13).
rank_value('A', 14).

% Define suit values for comparison (Spades > Hearts > Diamonds > Clubs)
suit_value(spades, 4).
suit_value(hearts, 3).
suit_value(diamonds, 2).
suit_value(clubs, 1).

% Predicate to determine if a player's card can beat the top card on the table
% Each card is represented as card(Rank, Suit)
valid_move(card(PlayerRank, PlayerSuit), card(TopRank, TopSuit)) :-
    rank_value(PlayerRank, PlayerValue),
    rank_value(TopRank, TopValue),
    (
        PlayerValue > TopValue  % Higher rank wins
        ;
        (PlayerValue == TopValue,  % If ranks are the same, compare suits
         suit_value(PlayerSuit, PlayerSuitValue),
         suit_value(TopSuit, TopSuitValue),
         PlayerSuitValue > TopSuitValue)
    ).
