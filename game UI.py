# Code for setting up a basic Pygame UI to integrate the existing Deck and Player classes.

import pygame
import sys
from Deck import Deck
from Player import Player
from typing import List
from collections import OrderedDict
from random import randint
from pyswip import Prolog
from time import sleep

class Game:
    def __init__(self):
        # initialize game settings
        self.state = "home"
        self.players: List[Player] = [Player("Player")]
        self.card_count: int = 16
        self.ai_count: int = 1
        self.selected_cards = []

        # initialize game ui
        pygame.init()
        self.buttons = []
        self.screen_width = 1400
        self.screen_height = 800
        self.caption = "Daifugō card game"
        self.screen = pygame.display.set_mode((self.screen_width, self.screen_height))
        pygame.display.set_caption(self.caption)
        self.dark_green = (2, 48, 32)
        self.light_green = ("#114f11")
        
        self.run_game()



    def draw_text(self, text, font, color, x, y):
        rendered_text = font.render(text, True, color)
        self.screen.blit(rendered_text, (x, y))

    def draw_button(self, text, x, y, width, height, color):
        mouse = pygame.mouse.get_pos()
        if x + width > mouse[0] > x and y + height > mouse[1] > y:
            pygame.draw.rect(self.screen, self.hover_color, (x, y, width, height))
        else:
            pygame.draw.rect(self.screen, color, (x, y, width, height))
        if text == "+" or text == "-":
            self.draw_text(text, self.adjust_font, self.text_color, x + 10, y)
        else:
            self.draw_text(text, self.button_font, self.text_color, x + 10, y)

    def check_button_click(self, pos):
        for button in self.buttons:
            if button["x"] + button["width"] > pos[0] > button["x"] and button["y"] + button["height"] > pos[1] > button["y"]:
                button["action"]()

    def increase_card_count(self):
        if self.card_count < 26:
            self.card_count += 1

    def decrease_card_count(self):
        if self.card_count > 1:
            self.card_count -= 1

    def increase_ai_count(self):
        if self.ai_count < 3:
            self.ai_count += 1

    def decrease_ai_count(self):
        if self.ai_count > 1:
            self.ai_count -= 1

    def play_game(self):
        # Initialize Prolog instance
        self.prolog = Prolog()
        # Load the Prolog game logic module
        self.prolog.consult("ai_logic.pl")
        self.prolog.consult("game_logic.pl")

        self.deck = Deck()
        self.played_cards = []
        self.num_played_cards = 0
        self.ai_query = False
        self.winner = None
        self.top_card = []
        start_card = self.deck.cards[0]
        self.innit_gameUI_data()
        self.state = "main_game"
        print("Game started with:")
        print(f"- Number of cards: {self.card_count}")
        print(f"- Number of AI: {self.ai_count}")
        for i in range(self.ai_count):
            self.players.append(Player(f"AI"))
        self.status_player = [True for i in range(len(self.players))]
        self.playable_player = len(self.players)
        self.deck.shuffle()
        self.deal_initial_cards()
        print(f"initialize_game({self.players[0].show_hand()}, {self.players[1].show_hand()})")
        list(self.prolog.query(f"initialize_game({self.players[0].show_hand()}, {self.players[1].show_hand()})"))


        if start_card in self.players[0].hand:
            self.current_player = self.players[0]
        elif start_card in self.players[1].hand:
            self.current_player = self.players[1]
        else:
            self.current_player = self.players[randint(0, 1)]

        self.current_turn = list(self.prolog.query("game_logic:current_turn(Player)"))[0]["Player"]
        if self.current_turn.lower() != self.current_player.name.lower():
            list(self.prolog.query("game_logic:skip_turn"))
        self.print_game_state()
        self.set_initial_card_positions()


    def deal_initial_cards(self):
        for i in range(self.card_count):
            self.players[0].receive_cards(self.deck.deal(1))
            self.players[1].receive_cards(self.deck.deal(1))
        for player in self.players:
            player.sort_hand(reverse=False)

    def draw_home_page(self):
        self.screen.fill(self.dark_green)
        self.adjust_font = pygame.font.SysFont('Helvetica', 25)
        self.button_font = pygame.font.SysFont('Helvetica', 30)
        self.small_font = pygame.font.SysFont('Helvetica', 20)
        self.font = pygame.font.SysFont('Helvetica', 40)
        self.button_color = (169, 169, 169)
        self.hover_color = (200, 200, 200)
        self.text_color = (255, 255, 255)
        center_x = self.screen_width // 2
        center_y = self.screen_height // 2
        self.buttons = [{ "text": "Start Game", "x": center_x - 100, "y": center_y / 5 *5, "width": 200, "height": 50, "action": self.play_game },
                        { "text": "Quit", "x": center_x - 100, "y": center_y / 5 *7, "width": 200, "height": 50, "action": sys.exit },
                        { "text": "+", "x": 240, "y": center_y - 100, "width": 30, "height": 30, "action": self.increase_card_count },
                        { "text": "-", "x": 270, "y": center_y - 100, "width": 30, "height": 30, "action": self.decrease_card_count }]
                        #{ "text": "+", "x": 230, "y": center_y - 60, "width": 30, "height": 30, "action": self.increase_ai_count },
                        #{ "text": "-", "x": 260, "y": center_y - 60, "width": 30, "height": 30, "action": self.decrease_ai_count }
        # Draw title
        self.draw_text("Daifugō card game", self.font, self.text_color, center_x - 125, center_y - 200)

        # Draw labels
        self.draw_text(f"Number of card: {self.card_count} (MAX 26)", self.small_font, self.text_color, 20, center_y - 100)
        # self.draw_text(f"Number of AI: {self.ai_count} (MAX 3)", self.small_font, self.text_color, 20, center_y - 60)

        # Draw buttons
        for button in self.buttons:
            self.draw_button(button["text"], button["x"], button["y"], button["width"], button["height"], self.button_color)

    def innit_gameUI_data(self):
        self.card_UIobj = OrderedDict()
        self.closed_card = pygame.transform.scale(pygame.image.load("Cards/red card.png"), (150, 200))
        for card in self.deck.cards:
            self.card_UIobj[card] = {"card_images": self.card_image(card), "card_positions": self.card_position(), "card_visability": True, "card_show": True}

    def draw_main_game(self):
        self.font = pygame.font.SysFont('Helvetica', 40)
        self.button_font = pygame.font.SysFont('Helvetica', 30)
        self.screen.fill(self.light_green)
        self.draw_cards()
        self.confirm_button = pygame.Rect((self.screen_width / 10 * 7.5) - 5, self.screen_height / 4 * 2.85, 100, 40)
        pygame.draw.rect(self.screen, (169, 169, 169), self.confirm_button)
        self.draw_text("Confirm", self.button_font, (255, 255, 255), self.screen_width / 10 * 7.5, self.screen_height / 4 * 2.85)
        self.skip_button = pygame.Rect((self.screen_width / 10 * 8.5) - 5, self.screen_height / 4 * 2.85, 100, 40)
        pygame.draw.rect(self.screen, (169, 169, 169), self.skip_button)
        self.draw_text("Skip", self.button_font, (255, 255, 255), self.screen_width / 10 * 8.5, self.screen_height / 4 * 2.85)
        self.back_button = pygame.Rect((self.screen_width / 10 *  8.5) - 5, self.screen_height / 4 * 0.75, 100, 40)
        pygame.draw.rect(self.screen, (169, 169, 169), self.back_button)
        self.draw_text("Back", self.button_font, (255, 255, 255), self.screen_width / 10 * 8.5, self.screen_height / 4 * 0.75)

        if self.current_player != self.players[0] and not self.ai_query:
            self.ai_play()




    def card_image(self, card):
        return pygame.transform.scale(pygame.image.load(f"Cards/{card.suit}_rm/{card.rank}.png"), (150, 200))
    
    def card_position(self):
        pos = (self.screen_width // 2) + 200, (self.screen_height // 2) - 100
        return {"current": pos, "target": pos}

    def card_visability(self, card, card_data):
        if card in self.players[0].hand or card in self.played_cards or card in self.top_card:
            return card_data["card_images"]
        else:
            return self.closed_card


    def set_initial_card_positions(self):
        for card, card_data in self.card_UIobj.items():
            if card in self.players[0].hand:
                card_data["card_positions"]["target"] = (self.players[0].hand.index(card) * ((self.screen_width-100) // len(self.players[0].hand)), 650)
            elif card in self.players[1].hand:
                card_data["card_positions"]["target"] = (self.players[1].hand.index(card) * ((self.screen_width-100) // len(self.players[1].hand)), -100)


    def draw_cards(self):
        for card, card_data in self.card_UIobj.items():
            self.draw_image_with_border(self.card_visability(card, self.card_UIobj[card]), card_data["card_positions"]["current"])
            card_data["card_positions"]["current"] = self.move_card_animation(card_data["card_positions"]["current"], card_data["card_positions"]["target"])
        
        
    def draw_image_with_border(self, image, pos, border_color=(0, 0, 0), border_width=2):
        # Draw the image
        self.screen.blit(image, pos)
        
        # Get the rect of the image for positioning
        rect:pygame.Rect = image.get_rect(topleft=pos)
        rect.x += 5
        rect.width -= 10
        
        # Draw the border
        pygame.draw.rect(self.screen, border_color, rect, border_width)
    
    def move_card_animation(self, start_pos, end_pos, speed=7):
        start_pos = list(start_pos)
        end_pos = list(end_pos)
        if start_pos[0] < end_pos[0]:
            start_pos[0] += min(speed, end_pos[0] - start_pos[0])
        elif start_pos[0] > end_pos[0]:
            start_pos[0] -= min(speed, start_pos[0] - end_pos[0])
        if start_pos[1] < end_pos[1]:
            start_pos[1] += min(speed, end_pos[1] - start_pos[1])
        elif start_pos[1] > end_pos[1]:
            start_pos[1] -= min(speed, start_pos[1] - end_pos[1])
        return tuple(start_pos)


    def check_card_click(self, pos):
        if self.current_player == self.players[0]: 
            for card, card_data in self.card_UIobj.items():
                card_rect = card_data["card_images"].get_rect(topleft=card_data["card_positions"]["current"])
                # check for last card in hand
                if card != self.players[0].hand[-1] and len(self.players[0].hand) > 1:
                    card_rect.width = abs(self.card_UIobj[self.players[0].hand[0]]["card_positions"]["current"][0] - self.card_UIobj[self.players[0].hand[1]]["card_positions"]["current"][0])
                card_rect.x += 5
                card_rect.width -= 10
                if card_rect.collidepoint(pos) and card in self.players[0].hand:
                    if card in self.selected_cards:
                        card_data["card_positions"]["target"] = (card_data["card_positions"]["current"][0], card_data["card_positions"]["current"][1] + 20)
                        self.selected_cards.remove(card)
                    else:
                        card_data["card_positions"]["target"] = (card_data["card_positions"]["current"][0], card_data["card_positions"]["current"][1] - 20)
                        self.selected_cards.append(card)

    def ai_play(self):
        self.ai_query = True
        print("AI is playing...")
        try:
            results = list(self.prolog.query("ai_turn(Move)"))
            results = [card.strip(",").strip() for card in results[0]['Move']]
        except:
            results = None
        if results:
            print("Results:", results)
            for card in self.current_player.hand:
                if f"({card.rank.lower()}, {card.suit.lower()})" in results:
                    self.selected_cards.append(card)
            self.confirm_selection()
        else:
            print("No valid move found.")
            self.skip_turn()


    def confirm_selection(self):
        if self.selected_cards:
            self.sort_confirm()
            self.num_played_cards = len(self.selected_cards)
            if self.current_player == self.players[0]:
                move = [card.format_card() for card in self.selected_cards]
                print(f"{self.current_player.name} played {move}")
                query_result = list(self.prolog.query(f"player_turn({move})"))
            else:
                query_result = True
            if query_result:
                for i, card in enumerate(self.selected_cards):
                    self.current_player.play_card(card)
                    self.top_card.append(card)
                    self.card_UIobj.move_to_end(card)
                    card_data = self.card_UIobj[card]
                    card_data["card_positions"]["target"] = ((self.screen_width // 2) - (100/len(self.selected_cards)*i) + randint(-3,3), (self.screen_height // 2) - 100 + randint(-5,5))
                self.ai_query = False
                self.check_winner()
                self.set_initial_card_positions()
                self.next_turn()
                self.selected_cards = []  # Clear selections after playing
            else:
                print("Invalid move. Try again.")


    def sort_confirm(self, re=False):
        # Sort the hand by card rank then suit but A is the second highest and 2 is the highest
        rank_order = {'2': 13, 'A': 12, 'K': 11, 'Q': 10, 'J': 9, '10': 8, '9': 7, '8': 6, '7': 5, '6': 4, 
              '5': 3, '4': 2, '3': 1}

        # Sort the hand by rank first, then by suit alphabetically
        self.selected_cards = sorted(self.selected_cards, key=lambda card: (rank_order[card.rank], card.suit), reverse=re)

    def next_turn(self):
        if self.playable_player > 1:
            self.current_player = self.players[(self.players.index(self.current_player) + 1) % len(self.players)]
        else:
            self.playable_player = len(self.players)
            self.current_player = self.players[self.status_player.index(True)]
            self.status_player = [True for i in range(len(self.players))]
            for card in self.top_card:
                self.card_UIobj[card]["card_positions"]["target"] = (self.card_UIobj[card]["card_positions"]["target"][0] - 200, self.card_UIobj[card]["card_positions"]["target"][1])
            self.played_cards.extend(self.top_card)
            self.top_card = []
        self.print_game_state()


    def skip_turn(self):
        if self.selected_cards:
            for card in self.selected_cards:
                self.card_UIobj[card]["card_positions"]["target"] = (self.card_UIobj[card]["card_positions"]["current"][0], self.card_UIobj[card]["card_positions"]["current"][1] + 20)
            self.selected_cards = []

        self.status_player[self.players.index(self.current_player)] = False
        self.playable_player -= 1
        list(self.prolog.query("game_logic:skip_turn"))
        print(f"{self.current_player.name} skipped their turn.")
        self.next_turn()
        


    def check_winner(self):
        game_end = list(self.prolog.query("check_game_end(Winner)"))
        if game_end:
            self.winner = self.current_player
            winner = game_end[0]["Winner"]
            print(f"\nGame over! The winner is: {winner}")
            self.ai_query = True

    def run_game(self):
        running = True
        while running:

            # Draw based on current state
            if self.state == "home":
                self.draw_home_page()
            elif self.state == "main_game":
                self.draw_main_game()
                if self.winner:
                    transparent_box = pygame.Surface((self.screen_width, self.screen_height), pygame.SRCALPHA)
                    transparent_box.fill((0, 0, 0, 128))
                    self.screen.blit(transparent_box, (0, 0))
                    self.draw_text(f"{self.winner.name} wins!", self.font, (255, 255, 255), self.screen_width // 2 - 100, self.screen_height // 2)

            # Event handling
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    if event.button == 1:  # Left click
                        if self.state == "home":
                            self.check_button_click(event.pos)
                        elif self.state == "main_game":
                            if self.confirm_button.collidepoint(event.pos):
                                self.confirm_selection()
                            elif self.skip_button.collidepoint(event.pos):
                                self.skip_turn()
                            elif self.back_button.collidepoint(event.pos):
                                self.state = "home"
                            else:
                                self.check_card_click(event.pos)

            pygame.display.flip()
            pygame.time.Clock().tick(120)

        pygame.quit()


    def print_game_state(self):
        # Get current player hands and turn
        player_hand = list(self.prolog.query("game_logic:player_hand(player, Hand)"))[0]["Hand"]
        ai_hand = list(self.prolog.query("game_logic:player_hand(ai, Hand)"))[0]["Hand"]
        current_turn = list(self.prolog.query("game_logic:current_turn(Player)"))[0]["Player"]
        last_play = list(self.prolog.query("game_logic:last_play(LastPlay)"))[0]["LastPlay"]
        print(f"\nCurrent turn: {current_turn} {self.current_player.name}")
        print(f"Player hand: {player_hand}")
        print(f"AI hand: {ai_hand}")
        print(f"Last play: {last_play}")



if __name__ == "__main__":
    Game()