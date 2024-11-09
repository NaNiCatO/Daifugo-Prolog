% Define rank values for comparison purposes
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





% Scoring strategy based on rank and suit
move_score(card(Rank, Suit), Score) :-
    rank_value(Rank, RankScore),
    suit_value(Suit, SuitScore),
    Score is ((14 - RankScore) * 10) + (4 - SuitScore),
    format('Scoring card ~w-~w: ~w~n', [Rank, Suit, Score]).


% Find the index of the highest score and select the best move
best_move(PlayerHand, TopCard, BestMove) :-
    findall(Move, (member(Move, PlayerHand), valid_move(Move, TopCard)), ValidMoves),
    maplist(move_score, ValidMoves, Scores),
    max_list(Scores, MaxScore),
    nth0(Index, Scores, MaxScore),
    nth0(Index, ValidMoves, BestMove).





% Existing rank and suit definitions and valid_move predicate remain unchanged

% Evaluation function for a given card (assessing its value)
evaluate_card(card(Rank, Suit), Score) :-
    rank_value(Rank, RankScore),
    suit_value(Suit, SuitScore),
    Score is RankScore + (SuitScore / 10).  % SuitScore added as a minor weight

% Evaluation function for an entire hand
evaluate_hand([], 0).  % Base case: no cards left to evaluate
evaluate_hand([Card | Rest], TotalScore) :-
    evaluate_card(Card, CardScore),
    evaluate_hand(Rest, RestScore),
    TotalScore is CardScore + RestScore.

% Alpha-beta pruning with a depth-limited search
alpha_beta(State, Depth, Alpha, Beta, Player, BestMove, Value) :-
    Depth > 0,
    findall(Move, valid_move(Move, State), Moves),  % Find all valid moves for the current state
    bounded_best(Moves, State, Depth, Alpha, Beta, Player, BestMove, Value).

% Helper predicate to find the best move within the bounds of alpha and beta
bounded_best([Move | Moves], State, Depth, Alpha, Beta, Player, BestMove, Value) :-
    make_move(State, Move, NewState),
    NextDepth is Depth - 1,
    switch_player(Player, Opponent),
    alpha_beta(NewState, NextDepth, -Beta, -Alpha, Opponent, _, MoveValue),
    ActualValue is -MoveValue,
    update_alpha_beta(ActualValue, Alpha, Beta, Move, BestMove, Value, Moves, State, Depth, Player).

% Update alpha-beta values based on the current best move
update_alpha_beta(ActualValue, Alpha, Beta, Move, BestMove, Value, Moves, State, Depth, Player) :-
    (ActualValue > Alpha -> 
        NewAlpha = ActualValue, 
        NewBestMove = Move;
        NewAlpha = Alpha, 
        NewBestMove = null),
    (NewAlpha >= Beta ->
        Value = NewAlpha;  % Prune remaining branches
        bounded_best(Moves, State, Depth, NewAlpha, Beta, Player, BestMove, Value)).

% A switch_player predicate to alternate between players (can be AI or opponent)
switch_player(ai, opponent).
switch_player(opponent, ai).

% Make a move (simulating card play, updates state)
make_move(State, card(Rank, Suit), NewState) :-
    % Logic for updating the game state after a move
    delete(State, card(Rank, Suit), NewState).  % Remove the card from the current hand
