from pyswip import Prolog

# Initialize Prolog and load the two modules
prolog = Prolog()
prolog.consult("game_logic.pl")  # Path to game logic file
prolog.consult("ai_logic.pl")    # Path to AI logic file

# Initialize the game state with the player and AI hands
def initialize_game(player_hand, ai_hand):
    """
    Initializes the game by setting the player's and AI's hands in Prolog, 
    and selecting a random starting player.
    """
    list_player_hand = f"{player_hand}"
    list_ai_hand = f"{ai_hand}"
    
    # Call Prolog's initialize_game/2 to set up the game state
    list(prolog.query(f"initialize_game({list_player_hand}, {list_ai_hand})"))
    print("Game initialized with player's and AI's hands.")

# Perform a player's move
def player_move(move):
    """
    Executes a player's move in Prolog, updating the game state if the move is valid.
    """
    list_move = f"{move}"
    
    # Query Prolog's player_turn/1 to handle the player's move
    result = list(prolog.query(f"player_turn({list_move})"))
    
    if result:
        print("Player's move was successful.")
    else:
        print("Invalid move.")

# Let the AI decide its best move using the Minimax algorithm with alpha-beta pruning
def ai_move():
    """
    Queries Prolog to get the AI's best move and update the game state.
    """
    # Retrieve the best move for the AI
    ai_query = list(prolog.query("ai_turn(Move)"))
    
    if ai_query:
        ai_move = ai_query[0]['Move']
        print(f"AI plays: {ai_move}")
    else:
        print("AI could not determine a valid move.")

# Check if the game has ended and declare the winner
def check_game_end():
    """
    Checks if there is a winner by querying Prolog's check_game_end/1.
    """
    end_query = list(prolog.query("check_game_end(Winner)"))
    
    if end_query:
        winner = end_query[0]['Winner']
        print(f"Game over! The winner is: {winner}.")
        return True
    return False

# Example usage flow
def main_game_loop():
    player_hand = [3, 4, 5]
    ai_hand = [6, 7, 8]
    
    # Initialize the game state
    initialize_game(player_hand, ai_hand)
    
    # Main loop to handle turns
    while True:
        # Player makes a move (example move [3])
        player_move([3])
        
        # Check if the game has ended
        if check_game_end():
            break
        
        # AI makes its move
        ai_move()
        
        # Check if the game has ended
        if check_game_end():
            break

# Run the main game loop
main_game_loop()
