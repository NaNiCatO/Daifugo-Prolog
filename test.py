import pygame

# Initialize Pygame
pygame.init()

# Screen setup
screen = pygame.display.set_mode((800, 600))
pygame.display.set_caption("Transparent Box Example")

# Create a surface for the transparent box
box_width, box_height = 800, 600
transparent_box = pygame.Surface((box_width, box_height), pygame.SRCALPHA)  # Use SRCALPHA for transparency

# Fill the surface with a semi-transparent color (RGBA)
transparent_box.fill((0, 128, 255, 128))  # Last value (128) is the alpha channel (0 = fully transparent, 255 = fully opaque)

# Main loop flag
running = True

while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    # Clear the screen
    screen.fill((255, 255, 255))

    # Blit (draw) the transparent box onto the screen
    screen.blit(transparent_box, (0, 0))  # Position it on the screen

    # Update the display
    pygame.display.flip()

# Quit Pygame
pygame.quit()
