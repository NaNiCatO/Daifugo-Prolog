:- module(ai_logic, [best_move/2]).

% Importing game logic predicates from main game file
:- use_module(game_logic).  % Assuming game logic predicates are in a file called `game_logic.pl`

% Minimax Algorithm with Alpha-Beta Pruning
% best_move(+GameState, -BestMove) - Determine the AI''s best move from the current game state.
best_move(GameState, BestMove) :-
    alpha_beta(GameState, -inf, inf, BestMove, _).

% Alpha-Beta Pruning Implementation
% alpha_beta(+GameState, +Alpha, +Beta, -BestMove, -Value)
alpha_beta(GameState, Alpha, Beta, BestMove, Value) :-
    GameState = state(AIHand, _, LastPlay),
    valid_moves(AIHand, LastPlay, Moves),
    best_move_from(Moves, GameState, Alpha, Beta, nil, BestMove, Value).

% Evaluate moves and prune based on alpha and beta
best_move_from([], _, Alpha, _, Move, Move, Alpha).  % No moves, return current best
best_move_from([Move|Moves], GameState, Alpha, Beta, TempBest, BestMove, Value) :-
    make_move(GameState, Move, NewGameState),
    min_value(NewGameState, Alpha, Beta, MoveValue),
    (
        MoveValue > Alpha
        -> NewAlpha = MoveValue,
           NewBest = Move
        ;  NewAlpha = Alpha,
           NewBest = TempBest
    ),
    (
        NewAlpha >= Beta
        -> BestMove = NewBest,
           Value = NewAlpha
        ;  best_move_from(Moves, GameState, NewAlpha, Beta, NewBest, BestMove, Value)
    ).

% Min layer of Minimax (human player''s simulated turn)
min_value(GameState, Alpha, Beta, Value) :-
    GameState = state(_, _, LastPlay),
    valid_moves([], LastPlay, Moves),  % Empty list, as AI has no access to PlayerHand
    worst_move_from(Moves, GameState, Alpha, Beta, inf, Value).

% Evaluate opponent''s moves and prune based on alpha and beta
worst_move_from([], _, _, _, Value, Value).  % No moves, return current worst
worst_move_from([Move|Moves], GameState, Alpha, Beta, TempWorst, Value) :-
    make_move(GameState, Move, NewGameState),
    max_value(NewGameState, Alpha, Beta, MoveValue),
    (
        MoveValue < Beta
        -> NewBeta = MoveValue
        ;  NewBeta = Beta
    ),
    (
        Alpha >= NewBeta
        -> Value = NewBeta
        ;  worst_move_from(Moves, GameState, Alpha, NewBeta, TempWorst, Value)
    ).

% Max layer of Minimax (AI''s turn)
max_value(GameState, Alpha, Beta, Value) :-
    GameState = state(AIHand, _, LastPlay),
    valid_moves(AIHand, LastPlay, Moves),
    best_move_from(Moves, GameState, Alpha, Beta, -inf, _, Value).

% Helper predicate to generate valid moves from a hand
% valid_moves(+Hand, +LastPlay, -Moves) - List of Moves from Hand that are valid after LastPlay
valid_moves(Hand, LastPlay, Moves) :-
    findall(Move, (subset(Move, Hand), valid_move(Move, LastPlay)), Moves).

% Helper predicate to make a move and generate the next game state
% make_move(+GameState, +Move, -NewGameState) - Generates a new game state after Move
make_move(state(AIHand, _, LastPlay), Move, state(NewAIHand, _, Move)) :-
    subtract(AIHand, Move, NewAIHand).

% Utility: subset(+Sublist, +List) - Checks if Sublist is a subset of List
subset([], _).
subset([Elem|SubTail], List) :-
    member(Elem, List),
    subset(SubTail, List).
