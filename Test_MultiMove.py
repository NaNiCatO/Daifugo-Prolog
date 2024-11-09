from pyswip import Prolog

prolog = Prolog()
prolog.consult("Move_Validation.pro")  # Load your Prolog file

# Define the current game state
ai_hand = [{'rank': '8', 'suit': 'hearts'}, {'rank': '2', 'suit': 'diamonds'}, {'rank': '10', 'suit': 'clubs'}]
top_card = {'rank': '7', 'suit': 'spades'}
depth = 3  # Limit depth for alpha-beta pruning

# Convert the hand and card to Prolog facts
ai_hand_prolog = "[card('8', hearts), card('2', diamonds), card('10', clubs)]"
top_card_prolog = "card('7', spades)"

# Call alpha_beta search from Python
query = f"alpha_beta({top_card_prolog}, {depth}, -1000, 1000, ai, BestMove, Value)"
result = list(prolog.query(query))

if result:
    best_move = result[0]['BestMove']
    print(f"Best move for AI: {best_move}")
else:
    print("No valid move found.")
