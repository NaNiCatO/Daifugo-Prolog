import random

class Card:
    def __init__(self, id, suit, rank, color):
        self.id = id
        self.suit = suit
        self.rank = rank
        self.color = color

    def __repr__(self):
        return f"card('{self.rank}', {self.suit.lower()})"


class Deck:
    def __init__(self):
        self.cards = []
        self.build_deck()

    def build_deck(self):
        """Creates a standard 52-card deck."""
        suits = ['Clubs', 'Diamonds', 'Hearts', 'Spades']
        ranks = ['3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A', '2']
        
        id_counter = 1
        for rank in ranks:
            for suit in suits:
                color = 'Red' if suit in ['Hearts', 'Diamonds'] else 'Black'
                card = Card(id_counter, suit, rank, color)
                self.cards.append(card)
                id_counter += 1

    def shuffle(self):
        """Shuffles the deck of cards."""
        random.shuffle(self.cards)

    def deal(self, num_cards):
        """Deals a specified number of cards from the deck."""
        if num_cards > len(self.cards):
            raise ValueError("Not enough cards in the deck to deal.")
        dealt_cards = self.cards[:num_cards]
        self.cards = self.cards[num_cards:]  # Remove dealt cards from the deck
        return dealt_cards

    def remaining_cards(self):
        """Returns the number of remaining cards in the deck."""
        return len(self.cards)

    def __repr__(self):
        return f"Deck of {len(self.cards)} cards"


# Example usage
if __name__ == "__main__":
    deck = Deck()       # Create a new deck of cards
    print(deck)        # Show the number of cards in the deck
    deck.shuffle()     # Shuffle the deck
    print(deck.deal(5))  # Deal 5 cards from the deck
    print(deck.remaining_cards())  # Show remaining cards in the deck
