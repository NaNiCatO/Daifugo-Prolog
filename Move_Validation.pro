% Define rank and suit values for comparison purposes
rank_value('2', 15).
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

suit_value(spades, 4).
suit_value(hearts, 3).
suit_value(diamonds, 2).
suit_value(clubs, 1).

% Helper predicates to extract rank and suit from a card
card_rank(card(Rank, _), Rank).
card_suit(card(_, Suit), Suit).

% Predicate to determine if a player's cards can beat the top cards on the table
% If TopCards is empty, the move is valid as long as all CardPlayed have the same rank
valid_move(CardPlayed, []) :-
    maplist(card_rank, CardPlayed, PlayerRanks),
    list_to_set(PlayerRanks, [_]).  % Ensure all ranks are the same in CardPlayed

% If CardPlayed has exactly two more cards than TopCards, ignore rank comparison
valid_move(CardPlayed, TopCards) :-
    length(CardPlayed, PlayerLength),
    length(TopCards, TopLength),
    PlayerLength =:= TopLength + 2,  % If Player has exactly 2 more cards
    % No rank comparison, just check that all cards in CardPlayed have the same rank
    maplist(card_rank, CardPlayed, PlayerRanks),
    list_to_set(PlayerRanks, [_]).  % Ensure all ranks in CardPlayed are the same

% If CardPlayed and TopCards have the same length, compare rank and suit
valid_move(CardPlayed, TopCards) :- 
    TopCards \= [],
    length(CardPlayed, Length),
    length(TopCards, Length),  % Ensure both plays have the same number of cards
    maplist(card_rank, CardPlayed, PlayerRanks),
    list_to_set(PlayerRanks, [PlayerRank]),  % Ensure all ranks in CardPlayed are the same
    rank_value(PlayerRank, PlayerValue),
    maplist(card_rank, TopCards, TopRanks),
    list_to_set(TopRanks, [TopRank]),   % Ensure all ranks in TopCards are the same
    rank_value(TopRank, TopValue),
    (
        PlayerValue > TopValue  % Check if CardPlayed has a higher rank
        ;
        (PlayerValue == TopValue,        % If ranks are the same, compare the highest suit in each
         maplist(card_suit, CardPlayed, PlayerSuits),
         maplist(card_suit, TopCards, TopSuits),
         maplist(suit_value, PlayerSuits, PlayerSuitValues),
         maplist(suit_value, TopSuits, TopSuitValues),
         max_list(PlayerSuitValues, MaxPlayerSuitValue),
         max_list(TopSuitValues, MaxTopSuitValue),
         MaxPlayerSuitValue > MaxTopSuitValue)
    ).

% Define the best move logic for the AI.
best_move(AiHand, TopCard, BestMove) :-
    length(TopCard, TopCardLength),
    (   TopCardLength == 0 ->  % If no card has been played yet
        findall(Move, (subset(Move, AiHand), valid_move(Move, [])), ValidMoves)  % Find any valid move
    ;   TopCardLength + 2 == length(Move) ->  % If TopCard length + 2 matches the move length
        findall(Move, (subset(Move, AiHand), valid_move(Move, TopCard)), ValidMoves)  % Find valid moves based on TopCard
    ),
    % Evaluate the moves and choose the best one
    maplist(move_score, ValidMoves, Scores),
    max_list(Scores, MaxScore),
    nth0(Index, Scores, MaxScore),
    nth0(Index, ValidMoves, BestMove).


% This move_score function will still calculate the score for combinations.
move_score(Move, Score) :-
    % Assuming each card in the Move contributes to the total score.
    % Modify this according to your game’s specific rules.
    maplist(card_rank, Move, Ranks),
    maplist(rank_value, Ranks, RankScores),
    sum_list(RankScores, TotalScore),
    Score is TotalScore.


