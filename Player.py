from Deck import Deck
from typing import List
class Player:
    def __init__(self, name):
        self.name = name
        self.hand:List[Deck] = []  # Player's hand of cards

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
    
    def sort_hand(self):
        # Sort the hand by card rank then suit but A is the second highest and 2 is the highest
        rank_order = {'2': 13, 'A': 12, 'K': 11, 'Q': 10, 'J': 9, '10': 8, '9': 7, '8': 6, '7': 5, '6': 4, 
              '5': 3, '4': 2, '3': 1}

        # Sort the hand by rank first, then by suit alphabetically
        self.hand = sorted(self.hand, key=lambda card: (rank_order[card.rank], card.suit))

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
    print(player1.show_hand())
    player1.sort_hand()
    print(player1.show_hand())

