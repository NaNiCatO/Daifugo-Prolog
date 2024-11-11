
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