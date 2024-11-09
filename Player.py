from Deck import Deck

class Player:
    def __init__(self, name):
        self.name = name
        self.hand = []  # Player's hand of cards

    def add_card(self, card):
        self.hand.append(card)

    def receive_cards(self, cards):
        """Adds received cards to the player's hand."""
        self.hand.extend(cards)

    def play_card(self, card):
        """Removes a specified card from the player's hand."""
        if card in self.hand:
            self.hand.remove(card)
            return card
        else:
            raise ValueError(f"{card} is not in {self.name}'s hand.")

    def show_hand(self):
        return [str(card) for card in self.hand]

    def __repr__(self):
        return f"Player: {self.name}, Hand: {len(self.hand)} cards"


# Example usage
if __name__ == "__main__":
    # Create a player
    player1 = Player("Alice")
    player2 = Player("Bob")

    # Create and shuffle a deck
    deck = Deck()
    deck.shuffle()

    # Deal cards to players
    player1.receive_cards(deck.deal(5))  # Give 5 cards to Alice
    player2.receive_cards(deck.deal(5))  # Give 5 cards to Bob

    # Show players' hands
    print(player1)
    print(player2)

    # Player plays a card
    played_card = player1.play_card(player1.hand[0])  # Alice plays the first card in her hand
    print(f"{player1.name} played {played_card}")
    print(player1)  # Show Alice's hand after playing a card
