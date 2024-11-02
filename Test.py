from pyswip import Prolog

# Initialize Prolog interface
prolog = Prolog()

# Load the Prolog file with game rules
prolog.consult("Move_Validation.pro")  # Make sure "Move_Validation.pro" is the correct filename and path

# Example cards
player_move = [("card", "7", "hearts")]
last_played = [("card", "5", "diamonds")]

# Convert Python data to Prolog format
def format_card_list(cards):
    return "[" + ",".join(f"card({rank}, {suit})" for (_, rank, suit) in cards) + "]"

def check_move_validity(player_move, last_played):
    # Format cards for Prolog query
    player_move_str = format_card_list(player_move)
    last_played_str = format_card_list(last_played)
    
    # Construct and run the query
    query = f"valid_move({player_move_str}, {last_played_str})"
    result = list(prolog.query(query))
    
    # Print the result
    if result:
        print("The move is valid!")
    else:
        print("The move is invalid.")

# Call the function to check move validity
check_move_validity(player_move, last_played)
