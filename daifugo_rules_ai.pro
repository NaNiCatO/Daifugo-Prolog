% Card rankings and values (green cut - once we find a rank value, stop looking)
rank_value('2', 2) :- !.
rank_value('3', 3) :- !.
rank_value('4', 4) :- !.
rank_value('5', 5) :- !.
rank_value('6', 6) :- !.
rank_value('7', 7) :- !.
rank_value('8', 8) :- !.
rank_value('9', 9) :- !.
rank_value('10', 10) :- !.
rank_value('J', 11) :- !.
rank_value('Q', 12) :- !.
rank_value('K', 13) :- !.
rank_value('A', 14) :- !.

% Suit hierarchy (green cut - once we find a suit value, stop looking)
suit_value(spades, 4) :- !.
suit_value(hearts, 3) :- !.
suit_value(diamonds, 2) :- !.
suit_value(clubs, 1) :- !.

% Compare cards using cuts to optimize decision tree
compare_cards(card(PlayerRank, PlayerSuit), card(TopRank, TopSuit), Result) :-
    rank_value(PlayerRank, PlayerValue),
    rank_value(TopRank, TopValue),
    (PlayerValue > TopValue -> 
        Result = higher, !  % Red cut - if rank is higher, no need to check suits
    ;
        PlayerValue < TopValue -> 
        Result = lower, !  % Red cut - if rank is lower, no need to check suits
    ;
        % Ranks are equal, compare suits
        suit_value(PlayerSuit, PlayerSuitValue),
        suit_value(TopSuit, TopSuitValue),
        (PlayerSuitValue > TopSuitValue -> 
            Result = higher, !
        ;
            Result = lower, !
        )
    ).

% Enhanced move validation with cuts
valid_move(card(PlayerRank, PlayerSuit), card(TopRank, TopSuit)) :-
    compare_cards(card(PlayerRank, PlayerSuit), card(TopRank, TopSuit), Result),
    Result = higher, !.  % Red cut - once we know it's higher, stop checking

% Validate multiple card plays with cuts for efficiency
valid_multiple_move(PlayerCards, TopCards) :-
    length(PlayerCards, N),
    length(TopCards, N), !,  % Green cut - if lengths don't match, fail immediately
    all_same_rank(PlayerCards),
    all_same_rank(TopCards),
    highest_card(PlayerCards, PlayerHighest),
    highest_card(TopCards, TopHighest),
    valid_move(PlayerHighest, TopHighest).

% Optimized same rank checking with cuts
all_same_rank([_]) :- !.  % Green cut - single card is always same rank
all_same_rank([card(Rank, _), card(Rank, _)|Rest]) :-
    all_same_rank([card(Rank, _)|Rest]), !.  % Green cut - avoid redundant checks

% Find highest card in set with cuts
highest_card([Card], Card) :- !.  % Green cut - single card is highest
highest_card([Card1, Card2|Rest], Highest) :-
    compare_cards(Card1, Card2, Result),
    (Result = higher ->
        highest_card([Card1|Rest], Highest), !  % Red cut - commit to higher card
    ;
        highest_card([Card2|Rest], Highest), !  % Red cut - commit to higher card
    ).

% AI strategy predicates with cuts for efficiency
evaluate_hand(Hand, Score) :-
    findall(Value, 
        (member(card(Rank, _), Hand),
         rank_value(Rank, Value)),
        Values),
    sum_list(Values, Score), !.  % Green cut - once we have the score, commit

% Optimized best move selection with cuts
best_move(Hand, TopCards, BestMove) :-
    findall(Move-Score,
        (generate_valid_move(Hand, TopCards, Move),
         evaluate_move(Move, Score)),
        Moves),
    sort(Moves, SortedMoves),  % Sort moves by score
    last(SortedMoves, BestMove-_), !.  % Red cut - commit to best move

% Generate valid moves more efficiently
generate_valid_move(Hand, TopCards, Move) :-
    subset(Hand, Move),
    (TopCards = [] -> 
        all_same_rank(Move), !  % Green cut - first play only needs same rank
    ;
        valid_multiple_move(Move, TopCards), !  % Red cut - commit to valid move
    ).

% Evaluate moves with cut for efficiency
evaluate_move(Move, Score) :-
    length(Move, Length),
    evaluate_hand(Move, HandScore),
    Score is HandScore * Length, !.  % Green cut - once score is calculated, commit

% Main interface predicates with cuts
verify_move(PlayerCards, TopCards) :-
    (TopCards = [] -> 
        all_same_rank(PlayerCards), !  % Green cut - first play validation
    ;
        valid_multiple_move(PlayerCards, TopCards), !  % Red cut - commit to validation result
    ).

% Get AI move with strategic planning and cuts
get_ai_move(Hand, TopCards, Move) :-
    (TopCards = [] ->
        find_best_initial_move(Hand, Move), !  % Red cut - commit to best initial move
    ;
        best_move(Hand, TopCards, Move), !  % Red cut - commit to best response move
    ).

% Optimize initial move selection
find_best_initial_move(Hand, Move) :-
    findall(Move-Score,
        (subset(Hand, Move),
         all_same_rank(Move),
         evaluate_move(Move, Score)),
        Moves),
    sort(Moves, SortedMoves),
    last(SortedMoves, Move-_), !.  % Red cut - commit to best initial move

% Strategic move evaluation with cuts
evaluate_strategy(Hand, Move, Score) :-
    evaluate_move(Move, MoveScore),
    remaining_hand_value(Hand, Move, RemainingScore),
    Score is MoveScore + RemainingScore, !.  % Green cut - commit to final score

% Calculate remaining hand value
remaining_hand_value(Hand, Move, Score) :-
    subtract(Hand, Move, Remaining),
    evaluate_hand(Remaining, Score), !.  % Green cut - commit to remaining value

% Utility predicates for hand analysis with cuts
count_sets(Hand, Sets) :-
    findall(Set,
        (subset(Hand, Set),
         length(Set, Len),
         Len > 1,
         all_same_rank(Set)),
        Sets), !.  % Green cut - once we have all sets, commit