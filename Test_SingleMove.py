from pyswip import Prolog

# Initialize the Prolog engine
prolog = Prolog()

# Load the Prolog file containing your move validation and selection logic
prolog.consult("Move_Validation.pro")

# Example player hand and top card in Python
player_hand = [
    {'rank': 3, 'suit': 'hearts'},
    {'rank': 5, 'suit': 'diamonds'},
    {'rank': 7, 'suit': 'spades'}
]
ai_hand = [
    {'rank': 2, 'suit': 'hearts'},
    {'rank': 6, 'suit': 'diamonds'},
    {'rank': 8, 'suit': 'spades'}
]
top_card = {'rank': 4, 'suit': 'clubs'}

# Helper function to format cards for Prolog
def format_card(card):
    return f"card('{card['rank']}', {card['suit']})"


# Convert player hand to Prolog-readable format
player_hand_prolog = [format_card(card) for card in player_hand]
top_card_prolog = format_card(top_card)


# Query Prolog for the best move
query = f"best_move([{', '.join(player_hand_prolog)}], {top_card_prolog}, BestMove)"
print("Query:", query)
results = list(prolog.query(query))
print("Results:", results)
# Print the best move if available
if results:
    best_move = results[0]['BestMove']
    print("Best move:", best_move)
else:
    print("No valid move found.")



# # Query Prolog for the valid move in each card in the player's hand
# valid_moves = []
# for card in player_hand:
#     card_prolog = format_card(card)
#     query = f"valid_move({card_prolog}, {top_card_prolog})"
#     print("Query:", query)
#     if list(prolog.query(query)):
#         valid_moves.append(card)

# print("Valid moves:", valid_moves)