% Card ranks and values (e.g., 2s are high, Jokers higher)
card_rank({'rank': Rank, 'suit': _}, RankValue) :-
    card_rank_value(Rank, RankValue).

% Example rank values (customize as per game rules)
card_rank_value(3, 3).
card_rank_value(4, 4).
card_rank_value(5, 5).
card_rank_value(6, 6).
card_rank_value(7, 7).
card_rank_value(8, 8).
card_rank_value(9, 9).
card_rank_value(10, 10).
card_rank_value('J', 11).
card_rank_value('Q', 12).
card_rank_value('K', 13).
card_rank_value('A', 14).
card_rank_value(2, 15). % Special high value for 2s
card_rank_value('joker', 16). % Highest value

% Suit priority for tie-breaking (customize as needed)
suit_priority('spades', 4).
suit_priority('hearts', 3).
suit_priority('diamonds', 2).
suit_priority('clubs', 1).



% valid_move(State, Player, Move)
valid_move(state(AIHand, TopCard, _), ai, Move) :-
    member(Move, AIHand), % Ensure the move is part of the AI's hand
    beats(Move, TopCard).

% Define what it means for a card to beat another card
beats(Card1, Card2) :-
    card_rank(Card1, Rank1),
    card_rank(Card2, Rank2),
    Rank1 > Rank2. % Simple rule: higher rank beats lower rank

% Include a rule for special cases if suits matter (optional)
beats({'rank': Rank, 'suit': Suit1}, {'rank': Rank, 'suit': Suit2}) :-
    suit_priority(Suit1, P1),
    suit_priority(Suit2, P2),
    P1 > P2. % Higher suit priority beats lower if ranks are equal


% make_move(State, Move, NewState)
make_move(state(AIHand, TopCard, PlayedCards), Move, state(NewAIHand, Move, NewPlayedCards)) :-
    select(Move, AIHand, NewAIHand), % Remove the played card from AI's hand
    append([Move], PlayedCards, NewPlayedCards). % Add the played card to the played cards list




% Alpha-beta pruning with evaluation function
alpha_beta(State, Depth, Alpha, Beta, Player, BestMove, Value) :-
    Depth > 0,
    findall(Move, valid_move(State, Player, Move), Moves),
    Moves \= [], % Ensure there are moves to consider
    bounded_best(Moves, State, Depth, Alpha, Beta, Player, BestMove, Value).

% If Depth is 0 or no more moves, evaluate the state
alpha_beta(State, 0, _, _, _, _, Value) :-
    evaluate_state(State, Value).

% Handle the case where there are no valid moves (e.g., passing)
alpha_beta(State, _, _, _, _, _, Value) :-
    \+ valid_move(State, _, _),
    evaluate_state(State, Value).

% Helper to find the best move
bounded_best([Move | Moves], State, Depth, Alpha, Beta, Player, BestMove, BestValue) :-
    make_move(State, Move, NewState),
    NextDepth is Depth - 1,
    switch_player(Player, Opponent),
    alpha_beta(NewState, NextDepth, -Beta, -Alpha, Opponent, _, MoveValue),
    ActualValue is -MoveValue,
    update_best(Move, ActualValue, Moves, State, Depth, Alpha, Beta, Player, BestMove, BestValue).

% Update the alpha or beta values and prune if necessary
update_best(Move, ActualValue, Moves, State, Depth, Alpha, Beta, Player, BestMove, BestValue) :-
    (ActualValue > Alpha ->
        NewAlpha = ActualValue,
        ChosenMove = Move;
        NewAlpha = Alpha,
        ChosenMove = null),
    (NewAlpha >= Beta ->
        BestMove = ChosenMove, BestValue = NewAlpha; % Prune
        bounded_best(Moves, State, Depth, NewAlpha, Beta, Player, NextBestMove, NextBestValue),
        (NextBestValue > ActualValue ->
            BestMove = NextBestMove, BestValue = NextBestValue;
            BestMove = ChosenMove, BestValue = NewAlpha)).
