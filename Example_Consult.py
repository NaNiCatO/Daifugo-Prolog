from pyswip import Prolog

# Initialize Prolog instance
prolog = Prolog()

# Load the Prolog game logic module
prolog.consult("game_logic.pl")
prolog.consult("ai_logic.pl")

# Function to print the current state of the game
def print_game_state():
    # Get current player hands and turn
    player_hand = list(prolog.query("game_logic:player_hand(player, Hand)"))[0]["Hand"]
    ai_hand = list(prolog.query("game_logic:player_hand(ai, Hand)"))[0]["Hand"]
    current_turn = list(prolog.query("game_logic:current_turn(Player)"))[0]["Player"]
    last_play = list(prolog.query("game_logic:last_play(LastPlay)"))[0]["LastPlay"]
    print(f"\nCurrent turn: {current_turn}")
    print(f"Player hand: {player_hand}")
    print(f"AI hand: {ai_hand}")
    print(f"Last play: {last_play}")

# Initialize game with example hands for player and AI
player_hand = [(3, 'hearts'), (4, 'spades'), (6, 'clubs'), (7, 'hearts'), (9, 'diamonds')]
ai_hand = [(3, 'diamonds'), (5, 'clubs'), (7, 'spades'), (10, 'hearts'), ("j", 'clubs')]

print("Initializing game...")
list(prolog.query(f"initialize_game({player_hand}, {ai_hand})"))
print(f"{player_hand}")
print_game_state()

# Simulate a few moves
turns = [
    {"player": [(4, 'spades')]},   # Player plays a single card
    {"ai": "Move"},        # AI plays a single card
    {"player": [(7, 'hearts')]},   # Player plays a single card
    {"ai": "Move"},      # AI plays a single card
    {"player": "skip"},            # Player skips
]

for i, turn in enumerate(turns):
    print(f"\nTurn {i+1}")
    if "player" in turn:
        if turn["player"] == "skip":
            list(prolog.query("game_logic:skip_turn"))
        else:
            move = turn["player"]
            list(prolog.query(f"player_turn({move})"))
    elif "ai" in turn:
        if turn["ai"] == "skip":
            list(prolog.query("game_logic:skip_turn"))
        else:
            move = turn["ai"]
            list(prolog.query(f"ai_turn({move})"))
    
    print_game_state()

    # Check if the game has ended after each turn
    game_end = list(prolog.query("check_game_end(Winner)"))
    if game_end:
        winner = game_end[0]["Winner"]
        print(f"\nGame over! The winner is: {winner}")
        break
else:
    print("\nGame is still ongoing.")
