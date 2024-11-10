# Code for setting up a basic Pygame UI to integrate the existing Deck and Player classes.

import pygame
import sys
from Deck import Deck
from Player import Player
from typing import List

class Game:
    def __init__(self):
        # initialize game settings
        self.state = "home"
        self.deck = Deck()
        self.deck.shuffle()
        self.players: List[Player] = [Player("Player")]
        self.card_count: int = 5
        self.ai_count: int = 1
        self.selected_cards = []

        # initialize game ui
        pygame.init()
        self.buttons = []
        self.screen_width = 1000
        self.screen_height = 600
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
        if self.card_count < 10:
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
        print("Game started with:")
        print(f"- Number of cards: {self.card_count}")
        print(f"- Number of AI: {self.ai_count}")
        for i in range(self.ai_count):
            self.players.append(Player(f"AI {i+1}"))
        self.deal_initial_cards()
        self.state = "main_game"
        self.innit_gameUI_data()


    def deal_initial_cards(self):
        for player in self.players:
            player.receive_cards(self.deck.deal(self.card_count))
            player.sort_hand()
            print(f"{player.name}: {player.show_hand()}")

    def play_round(self):
        for player in self.players:
            print(f"{player.name}'s turn:")
            card = player.play_card()
            print(f"Played: {card}")

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
        self.buttons = [{ "text": "Start Game", "x": center_x - 100, "y": 200, "width": 200, "height": 50, "action": self.play_game },
                        { "text": "Quit", "x": center_x - 100, "y": 300, "width": 200, "height": 50, "action": sys.exit },
                        { "text": "+", "x": 230, "y": center_y - 100, "width": 30, "height": 30, "action": self.increase_card_count },
                        { "text": "-", "x": 260, "y": center_y - 100, "width": 30, "height": 30, "action": self.decrease_card_count },
                        { "text": "+", "x": 230, "y": center_y - 60, "width": 30, "height": 30, "action": self.increase_ai_count },
                        { "text": "-", "x": 260, "y": center_y - 60, "width": 30, "height": 30, "action": self.decrease_ai_count }]
        # Draw title
        self.draw_text("Daifugō card game", self.font, self.text_color, center_x - 125, center_y - 200)

        # Draw labels
        self.draw_text(f"Number of card: {self.card_count} (MAX 10)", self.small_font, self.text_color, 20, center_y - 100)
        self.draw_text(f"Number of AI: {self.ai_count} (MAX 3)", self.small_font, self.text_color, 20, center_y - 60)

        # Draw buttons
        for button in self.buttons:
            self.draw_button(button["text"], button["x"], button["y"], button["width"], button["height"], self.button_color)

    def innit_gameUI_data(self):
        self.card_ui = {}
        self.closed_card = pygame.transform.scale(pygame.image.load("Cards/red card.png"), (150, 200))
        for player in self.players:
            self.card_ui[player.name] = {"card_images": self.card_image(player.hand), "card_positions": self.card_position(len(player.hand))}

    def draw_main_game(self):
        self.font = pygame.font.SysFont('Helvetica', 40)
        self.button_font = pygame.font.SysFont('Helvetica', 30)
        self.screen.fill(self.light_green)
        self.draw_cards()
        self.confirm_button = pygame.Rect(825, 375, 100, 40)
        pygame.draw.rect(self.screen, (169, 169, 169), self.confirm_button)
        self.draw_text("Confirm", self.button_font, (255, 255, 255), 830, 380)


    def card_image(self, hand):
        arr_card_images = []
        for card in hand:
            arr_card_images.append(pygame.transform.scale(pygame.image.load(f"Cards/{card.suit}_rm/{card.rank}.png"), (150, 200)))
        return arr_card_images
    
    def card_position(self, length):
        arr_card_positions = []
        for i in range(length):
            arr_card_positions.append((150 + i*60, 450))
        return arr_card_positions
    
    def draw_cards(self):
        for player in self.players:
            for i, (image, pos) in enumerate(zip(self.card_ui[player.name]["card_images"], self.card_ui[player.name]["card_positions"])):
                if player.name == "Player":
                    offset_y = -20 if i in self.selected_cards else 0
                    self.draw_image_with_border(image, (pos[0], pos[1] + offset_y))
                else:
                    self.draw_image_with_border(self.closed_card, (pos[0], -100))

    def draw_image_with_border(self, image, pos, border_color=(0, 0, 0), border_width=2):
        # Draw the image
        self.screen.blit(image, pos)
        
        # Get the rect of the image for positioning
        rect:pygame.Rect = image.get_rect(topleft=pos)
        rect.x += 5
        rect.width -= 10
        
        # Draw the border
        pygame.draw.rect(self.screen, border_color, rect, border_width)
    
    def move_card_animation(self, image, start_pos, end_pos, speed=5):
        x, y = start_pos
        target_x, target_y = end_pos

        while (x, y) != (target_x, target_y):

            # Calculate the direction and step
            if x < target_x:
                x += min(speed, target_x - x)
            elif x > target_x:
                x -= min(speed, x - target_x)

            if y < target_y:
                y += min(speed, target_y - y)
            elif y > target_y:
                y -= min(speed, y - target_y)

            # Draw the card at the new position
            self.screen.blit(image, (x, y))

            # Update the display
            pygame.display.flip()

            # Control the frame rate
            pygame.time.Clock().tick(60)


    def check_card_click(self, pos):
        for i, rect_pos in enumerate(self.card_ui["Player"]["card_positions"]):
            # check if last card
            if i == len(self.card_ui["Player"]["card_positions"]) - 1:
                rect = pygame.Rect(rect_pos[0], rect_pos[1], 150, 200)
            else:
                rect = pygame.Rect(rect_pos[0], rect_pos[1], 70, 200)
            if rect.collidepoint(pos):
                if i in self.selected_cards:
                    self.selected_cards.remove(i)
                else:
                    self.selected_cards.append(i)
                break

    def confirm_selection(self):
        if self.selected_cards:
            print("Playing cards:", self.selected_cards)
            self.selected_cards = []  # Clear selections after playing

    def run_game(self):
        running = True
        while running:

            # Draw based on current state
            if self.state == "home":
                self.draw_home_page()
            elif self.state == "main_game":
                self.draw_main_game()


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
                            else:
                                self.check_card_click(event.pos)

            pygame.display.flip()

        pygame.quit()


if __name__ == "__main__":
    Game()