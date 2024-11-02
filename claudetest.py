from typing import List, Optional
from pyswip import Prolog
import random

class Card:
    def __init__(self, id: int, suit: str, rank: str, value: int, color: str):
        self.id = id
        self.suit = suit
        self.rank = rank
        self.value = value
        self.color = color

    def to_prolog_term(self) -> str:
        """Convert card to Prolog term format"""
        return f"card('{self.rank}', {self.suit.lower()})"

    def __repr__(self):
        return f"{self.rank} of {self.suit}"

class Deck:
    def __init__(self):
        self.cards = []
        self.build_deck()

    def build_deck(self):
        """Creates a standard 52-card deck."""
        suits = ['Hearts', 'Diamonds', 'Clubs', 'Spades']
        ranks = ['2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A']
        
        id_counter = 1
        for suit in suits:
            for rank in ranks:
                value = ranks.index(rank) + 2
                color = 'Red' if suit in ['Hearts', 'Diamonds'] else 'Black'
                card = Card(id_counter, suit, rank, value, color)
                self.cards.append(card)
                id_counter += 1

    def shuffle(self):
        """Shuffles the deck of cards."""
        random.shuffle(self.cards)

    def deal(self, num_cards: int) -> List[Card]:
        """Deals a specified number of cards from the deck."""
        if num_cards > len(self.cards):
            raise ValueError("Not enough cards in the deck to deal.")
        dealt_cards = self.cards[:num_cards]
        self.cards = self.cards[num_cards:]
        return dealt_cards

    def remaining_cards(self) -> int:
        """Returns the number of remaining cards in the deck."""
        return len(self.cards)

    def __repr__(self):
        return f"Deck of {len(self.cards)} cards"

class PrologInterface:
    def __init__(self):
        self.prolog = Prolog()
        self.prolog.consult("daifugo_rules_ai.pro")

    def verify_move(self, player_cards: List[Card], top_cards: List[Card]) -> bool:
        """Verify if a move is valid using Prolog rules"""
        try:
            player_cards_pl = [card.to_prolog_term() for card in player_cards]
            top_cards_pl = [card.to_prolog_term() for card in top_cards]
            
            query = f"verify_move([{','.join(player_cards_pl)}], [{','.join(top_cards_pl)}])"
            return bool(list(self.prolog.query(query)))
        except Exception as e:
            print(f"Prolog verification error: {e}")
            return False

    def get_ai_move(self, hand: List[Card], top_cards: List[Card]) -> Optional[List[Card]]:
        """Get AI's suggested move using Prolog"""
        try:
            hand_pl = [card.to_prolog_term() for card in hand]
            top_cards_pl = [card.to_prolog_term() for card in top_cards]
            
            query = f"get_ai_move([{','.join(hand_pl)}], [{','.join(top_cards_pl)}], Move)"
            solutions = list(self.prolog.query(query, maxresult=1))  # Limit to one solution
            
            if solutions:
                move_pl = solutions[0]["Move"]
                return self._match_cards_from_prolog(move_pl, hand)
            return None
        except Exception as e:
            print(f"Prolog AI move error: {e}")
            return None

    def _match_cards_from_prolog(self, prolog_move, hand: List[Card]) -> List[Card]:
        """Match Prolog move terms to actual Card objects from hand"""
        move_cards = []
        for term in prolog_move:
            rank = str(term.args[0])
            suit = str(term.args[1]).capitalize()
            # Find matching card in hand
            for card in hand:
                if card.rank == rank and card.suit.lower() == suit.lower() and card not in move_cards:
                    move_cards.append(card)
                    break
        return move_cards


class Player:
    def __init__(self, name: str, is_ai: bool = False):
        self.name = name
        self.hand: List[Card] = []
        self.is_ai = is_ai
        self.prolog_interface = PrologInterface() if is_ai else None

    def receive_cards(self, cards: List[Card]):
        """Adds received cards to the player's hand."""
        self.hand.extend(cards)

    def play_cards(self, cards: List[Card]) -> List[Card]:
        """Removes specified cards from the player's hand."""
        for card in cards:
            if card not in self.hand:
                raise ValueError(f"{card} is not in {self.name}'s hand.")
            self.hand.remove(card)
        return cards

    def make_move(self, top_cards: List[Card]) -> Optional[List[Card]]:
        """Make a move based on whether player is AI or human"""
        if not self.is_ai:
            return None

        try:
            move = self.prolog_interface.get_ai_move(self.hand, top_cards)
            if move:
                return self.play_cards(move)
            return None
        except Exception as e:
            print(f"Error making AI move: {e}")
            # Fallback: play lowest valid single card if possible
            return self._fallback_move(top_cards)
        
    def _fallback_move(self, top_cards: List[Card]) -> Optional[List[Card]]:
        """Fallback strategy: play lowest valid single card"""
        if not top_cards:
            lowest_card = min(self.hand, key=lambda c: c.value)
            return self.play_cards([lowest_card])
            
        for card in sorted(self.hand, key=lambda c: c.value):
            if self.can_play([card], top_cards):
                return self.play_cards([card])
        return None

    def can_play(self, cards: List[Card], top_cards: List[Card]) -> bool:
        """Verify if selected cards make a valid play"""
        if not self.prolog_interface:
            self.prolog_interface = PrologInterface()
        return self.prolog_interface.verify_move(cards, top_cards)

    def show_hand(self) -> List[Card]:
        """Returns a list of cards in the player's hand."""
        return self.hand

    def __repr__(self):
        return f"Player: {self.name}, Hand: {len(self.hand)} cards"

# Example game setup and usage
def example_game():
    # Initialize game components
    deck = Deck()
    deck.shuffle()
    
    # Create players
    human = Player("Human", is_ai=False)
    ai = Player("AI", is_ai=True)
    
    # Deal initial hands
    human.receive_cards(deck.deal(5))
    ai.receive_cards(deck.deal(5))
    
    # Example turn
    top_cards = []  # No cards on table initially
    
    # Human turn example
    print(f"Human hand: {human.show_hand()}")
    selected_cards = [human.hand[0]]  # Example: try to play first card
    
    if human.can_play(selected_cards, top_cards):
        played_cards = human.play_cards(selected_cards)
        print(f"Human played: {played_cards}")
        top_cards = played_cards
    else:
        print("Invalid move!")
    
    # AI turn
    ai_move = ai.make_move(top_cards)
    if ai_move:
        print(f"AI played: {ai_move}")
        top_cards = ai_move

if __name__ == "__main__":
    example_game()