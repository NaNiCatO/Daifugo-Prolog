import pygame
import sys

class Game:
    def __init__(self):
        # Initialize Pygame
        pygame.init()
        
        # Screen setup
        self.screen = pygame.display.set_mode((800, 600))
        pygame.display.set_caption("Daifugō card game")
        self.font = pygame.font.SysFont('Helvetica', 24)

        # Game state management
        self.state = "home"  # Possible states: "home", "main_game"

        # Button for home page
        self.play_button = pygame.Rect(350, 250, 100, 50)  # Adjust size and position as needed

        # Load card images (replace with actual file paths)
        self.card_images = [pygame.image.load(f"Cards/Clubs_rm/{i}.png") for i in range(2, 7)]  # Example card images
        self.card_positions = [(150 + i*60, 500) for i in range(len(self.card_images))]
        
        # Deck and button attributes for the game page
        self.deck_image = pygame.image.load("Cards/red card.png")  # Replace with deck image file path
        self.deck_position = (600, 250)
        self.confirm_button = pygame.Rect(650, 500, 100, 40)
        
        # Game state
        self.selected_cards = []  # List of selected card indexes

        self.run_game()

    def draw_text(self, text, font, color, x, y):
        rendered_text = font.render(text, True, color)
        self.screen.blit(rendered_text, (x, y))

    def draw_home_page(self):
        # Draw the home page content
        self.draw_text("Daifugō card game", self.font, (255, 255, 255), 300, 100)
        pygame.draw.rect(self.screen, (169, 169, 169), self.play_button)
        self.draw_text("PLAY", self.font, (255, 255, 255), 375, 265)

    def draw_cards(self):
        # Draw player cards for the main game
        for i, (image, pos) in enumerate(zip(self.card_images, self.card_positions)):
            # Offset the position if the card is selected
            offset_y = -20 if i in self.selected_cards else 0
            self.screen.blit(image, (pos[0], pos[1] + offset_y))

    def draw_button(self):
        # Draw confirm button in the main game
        pygame.draw.rect(self.screen, (169, 169, 169), self.confirm_button)
        self.draw_text("Confirm", self.font, (255, 255, 255), 655, 505)

    def check_card_click(self, pos):
        for i, rect_pos in enumerate(self.card_positions):
            rect = pygame.Rect(rect_pos[0], rect_pos[1], 50, 70)  # Replace with card size
            if rect.collidepoint(pos):
                if i in self.selected_cards:
                    self.selected_cards.remove(i)
                else:
                    self.selected_cards.append(i)
                break

    def check_deck_click(self, pos):
        deck_rect = pygame.Rect(self.deck_position[0], self.deck_position[1], 50, 70)  # Replace with deck size
        if deck_rect.collidepoint(pos):
            print("Drawing a card...")  # Add draw logic here

    def confirm_selection(self):
        if self.selected_cards:
            print("Playing cards:", self.selected_cards)
            self.selected_cards = []  # Clear selections after playing

    def run_game(self):
        running = True
        while running:
            self.screen.fill((34, 94, 34))  # Background color

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    if event.button == 1:  # Left click
                        if self.state == "home":
                            if self.play_button.collidepoint(event.pos):
                                self.state = "main_game"  # Switch to the main game screen
                        elif self.state == "main_game":
                            if self.confirm_button.collidepoint(event.pos):
                                self.confirm_selection()
                            else:
                                self.check_card_click(event.pos)
                                self.check_deck_click(event.pos)

            # Draw based on current state
            if self.state == "home":
                self.draw_home_page()
            elif self.state == "main_game":
                self.screen.blit(self.deck_image, self.deck_position)
                self.draw_cards()
                self.draw_button()

            pygame.display.flip()

        pygame.quit()

if __name__ == "__main__":
    Game()
