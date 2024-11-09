from pyswip import Prolog

# Initialize Prolog
prolog = Prolog()

# Load the Prolog code into Python (assume code is saved in 'valid_move_rules.pl')
prolog.consult("Move_Validation.pro")

# Define the current game state
top_card_rank = '10'
top_card_suit = 'hearts'
player_card_rank = '10'
player_card_suit = 'spades'

print(f"valid_move(card('{player_card_rank}', {player_card_suit}), card('{top_card_rank}', {top_card_suit}))")
# Query Prolog to check if it's a valid move
query_result = list(prolog.query(
    f"valid_move(card('{player_card_rank}', {player_card_suit}), card('{top_card_rank}', {top_card_suit}))"
))


# Output result
if query_result:
    print(f"Playing {player_card_rank} of {player_card_suit} against {top_card_rank} of {top_card_suit} is a valid move.")
else:
    print(f"Playing {player_card_rank} of {player_card_suit} against {top_card_rank} of {top_card_suit} is NOT a valid move.")
