:- module(game_logic, [
    initialize_game/2,
    valid_move/2,
    player_turn/1,
    ai_turn/1,
    check_game_end/1,
    next_player_turn/0
]).

% Define card ranks for the standard gameplay order, with suits ranked: clubs < diamonds < hearts < spades
rank_order([('3', clubs), ('3', diamonds), ('3', hearts), ('3', spades),
            ('4', clubs), ('4', diamonds), ('4', hearts), ('4', spades),
            ('5', clubs), ('5', diamonds), ('5', hearts), ('5', spades),
            ('6', clubs), ('6', diamonds), ('6', hearts), ('6', spades),
            ('7', clubs), ('7', diamonds), ('7', hearts), ('7', spades),
            ('8', clubs), ('8', diamonds), ('8', hearts), ('8', spades),
            ('9', clubs), ('9', diamonds), ('9', hearts), ('9', spades),
            ('10', clubs), ('10', diamonds), ('10', hearts), ('10', spades),
            ('j', clubs), ('j', diamonds), ('j', hearts), ('j', spades),
            ('q', clubs), ('q', diamonds), ('q', hearts), ('q', spades),
            ('k', clubs), ('k', diamonds), ('k', hearts), ('k', spades),
            ('a', clubs), ('a', diamonds), ('a', hearts), ('a', spades),
            ('2', clubs), ('2', diamonds), ('2', hearts), ('2', spades)]).

% Game state facts
:- dynamic player_hand/2.         % player_hand(PlayerType, Hand) - Stores the hand for each player type
:- dynamic last_play/1.            % last_play(Cards) - Tracks the last play in the game
:- dynamic current_turn/1.         % current_turn(PlayerType) - Tracks whose turn it is (player or ai)

% Initialize the Game
% initialize_game(+PlayerHand, +AIHand) - Sets up the game state with hands for the player and AI
initialize_game(PlayerHand, AIHand) :-
    assert(player_hand(player, PlayerHand)),
    assert(player_hand(ai, AIHand)),
    random_between(0, 1, FirstPlayer), % Randomly determine the first player (0 for player, 1 for AI)
    (FirstPlayer = 0 -> assert(current_turn(player)); assert(current_turn(ai))),
    assert(last_play([])).

% Move Validation
% valid_move(+Move, +LastPlay) - Checks if a Move is valid based on the last play
valid_move(Move, LastPlay) :-
    player_hand(_, Hand),
    subset(Move, Hand),  % Ensure Move is a subset of the players hand
    length(Move, MoveLen),
    (   % Case 1: LastPlay is empty (initial move) - any valid move is allowed
        LastPlay == [] ->
        (   MoveLen =:= 1  % Allow any single card
        ;   MoveLen > 1, same_value_cards(Move)  % Allow multiple cards if they have the same rank
        )
    ;   % Case 2: LastPlay is not empty, so standard validation applies
        length(LastPlay, LastPlayLen),
        (   % Case 2.1: Same number of cards, check if value is higher
            MoveLen =:= LastPlayLen ->
            valid_single_or_same_size_move(Move, LastPlay)
        ;   % Case 2.2: Move has more cards (exactly 2 more)
            MoveLen =:= LastPlayLen + 2 ->
            same_value_cards(Move)
        )
    ).

% Case 1: Valid move for single card or matching number of cards
% valid_single_or_same_size_move(+Move, +LastPlay) - Checks if Move has higher rank than LastPlay
valid_single_or_same_size_move(Move, LastPlay) :-
    rank_order(RankOrder),
    hand_rank(Move, RankOrder, MoveRank),
    hand_rank(LastPlay, RankOrder, LastPlayRank),
    MoveRank > LastPlayRank,
    same_value_cards(Move).

% Check if all cards in the hand have the same value
% same_value_cards(+Hand) - Ensures all cards in Hand have the same rank
same_value_cards([Card | Cards]) :-
    maplist(same_rank(Card), Cards).

same_rank((Rank, _Suit), (Rank, _)).


% Determine rank of a hand in the order list (modified for single-card comparison)
% hand_rank(+Hand, +RankOrder, -Rank) - Retrieves the rank of the first card in Hand in RankOrder
hand_rank([Card|_], RankOrder, Rank) :-
    nth0(Rank, RankOrder, Card).

% Determine rank of hand in the order list
% Modified to handle tuples (Rank, Suit)
hand_rank([Card|_], RankOrder, Rank) :-
    nth0(Rank, RankOrder, Card).

% Players Turn
% player_turn(+Move) - Handles the players turn, ensuring valid moves and preventing multiple executions
player_turn(Move) :-
    current_turn(player),
    last_play(LastPlay),
    once(valid_move(Move, LastPlay)),      % Ensures valid_move succeeds only once
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

skip_turn :-
    current_turn(Current),
    (Current = player -> Next = ai; Next = player),
    retract(current_turn(Current)),
    assert(current_turn(Next)),
    retractall(last_play(_)),
    assert(last_play([])).