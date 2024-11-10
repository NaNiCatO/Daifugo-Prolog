import pygame

# Initialize Pygame
pygame.init()

# Screen setup
screen = pygame.display.set_mode((800, 600))
pygame.display.set_caption("Simple Animation Example")

# Card properties
card_color = (0, 128, 255)
card_size = (50, 100)
card_pos = [50, 250]  # Starting position
speed = 2  # Speed of the card's movement

# Main loop flag
running = True

# Main animation loop
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    # Clear the screen
    screen.fill((255, 255, 255))

    # Update the card position
    card_pos[0] += speed  # Move the card to the right

    # If the card reaches the edge of the screen, reverse direction
    if card_pos[0] + card_size[0] > 800 or card_pos[0] < 0:
        speed = -speed

    # Draw the card
    card_rect = pygame.Rect(card_pos, card_size)
    pygame.draw.rect(screen, card_color, card_rect)

    # Update the display
    pygame.display.flip()

    # Control the frame rate
    pygame.time.Clock().tick(60)

# Quit Pygame
pygame.quit()
