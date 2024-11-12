:- module(game_logic, [
    initialize_game/2,
    valid_move/2,
    player_turn/1,
    ai_turn/1,
    check_game_end/1,
    next_player_turn/0
]).

% Define card ranks for the standard gameplay order
rank_order([3, 4, 5, 6, 7, 8, 9, 10, j, q, k, a, 2]).

% Game state facts
:- dynamic player_hand/2.         % player_hand(PlayerType, Hand) - Stores the hand for each player type
:- dynamic last_play/1.            % last_play(Cards) - Tracks the last play in the game
:- dynamic current_turn/1.         % current_turn(PlayerType) - Tracks whose turn it is (player or ai)

% Initialize the Game
% initialize_game(+PlayerHand, +AIHand) - Sets up the game state with hands for the player and AI
initialize_game(PlayerHand, AIHand) :-
    assert(player_hand(player, PlayerHand)),
    assert(player_hand(ai, AIHand)),
    random_between(0, 1, FirstPlayer),
    (FirstPlayer = 0 -> assert(current_turn(player)); assert(current_turn(ai))),
    retractall(last_play(_)).

% Move Validation
% valid_move(+Move, +LastPlay) - Checks if a Move is valid based on the last play
valid_move(Move, LastPlay) :-
    player_hand(_, Hand),
    subset(Move, Hand),                    % Ensure Move is a subset of the players Hand
    rank_order(RankOrder),
    compare_hands(Move, LastPlay, RankOrder).

% Compare Move with Last Play
% compare_hands(+Move, +LastPlay, +RankOrder) - Checks if Move has a higher rank than LastPlay
compare_hands(Move, LastPlay, RankOrder) :-
    hand_rank(Move, RankOrder, MoveRank),
    hand_rank(LastPlay, RankOrder, LastPlayRank),
    MoveRank > LastPlayRank.

% Determine rank of hand in the order list
hand_rank([Card|_], RankOrder, Rank) :-
    nth0(Rank, RankOrder, Card).

% Players Turn
% player_turn(+Move) - Handles the players turn, ensuring valid moves
player_turn(Move) :-
    current_turn(player),
    last_play(LastPlay),
    valid_move(Move, LastPlay),
    play_move(player, Move),
    next_player_turn.

% AIs Turn
% ai_turn(-Move) - Determines the AIs move and plays it
ai_turn(Move) :-
    current_turn(ai),
    player_hand(ai, AIHand),
    last_play(LastPlay),
    ai_logic:best_move(state(AIHand, _, LastPlay), Move),
    play_move(ai, Move),
    next_player_turn.

% Play Move
% play_move(+PlayerType, +Move) - Updates the game state after a player makes a Move
play_move(PlayerType, Move) :-
    player_hand(PlayerType, Hand),
    subtract(Hand, Move, NewHand),
    retract(player_hand(PlayerType, Hand)),
    assert(player_hand(PlayerType, NewHand)),
    retractall(last_play(_)),
    assert(last_play(Move)).

% Turn Transition
% next_player_turn - Switches the turn between the player and the AI
next_player_turn :-
    current_turn(Current),
    (Current = player -> Next = ai; Next = player),
    retract(current_turn(Current)),
    assert(current_turn(Next)).

% Game End Check
% check_game_end(-Winner) - Checks if any player has won by clearing their hand
check_game_end(Winner) :-
    player_hand(Winner, Hand),
    Hand == [].

% Utility Predicate: Subset Check
% subset(+Sublist, +List) - Checks if Sublist is contained in List
subset([], _).
subset([Elem|SubTail], List) :-
    member(Elem, List),
    subset(SubTail, List).

% Reset Game State
% reset_game - Clears all dynamic facts for a new game round
reset_game :-
    retractall(player_hand(_, _)),
    retractall(last_play(_)),
    retractall(current_turn(_)).

