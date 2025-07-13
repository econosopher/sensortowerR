# regenerate_sticker.R

# Load necessary libraries
library(hexSticker)
library(ggplot2)
library(grid)
library(showtext)

# Enable showtext
showtext_auto()

# Add the chosen font
font_add_google("Roboto", "roboto")

# Create the central plot (same as above)
p <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1.6),
            fill = "#2C3E50", color = "#2C3E50", size = 2,
            corner_radius = grid::unit(0.1, "npc")) +
  geom_rect(aes(xmin = 0.1, xmax = 0.9, ymin = 0.2, ymax = 1.4),
            fill = "#ECF0F1", color = NA) +
  geom_rect(aes(xmin = 0.2, xmax = 0.28, ymin = 0.3, ymax = 1.2),
            fill = "#3498DB", color = "#2980B9") +
  geom_rect(aes(xmin = 0.35, xmax = 0.43, ymin = 0.5, ymax = 1.0),
            fill = "#E74C3C", color = "#C0392B") +
  geom_rect(aes(xmin = 0.5, xmax = 0.58, ymin = 0.4, ymax = 0.9),
            fill = "#F1C40F", color = "#F39C12") +
  geom_rect(aes(xmin = 0.45, xmax = 0.55, ymin = 1.6, ymax = 1.65),
            fill = "#2C3E50", color = "#2C3E50") +
  geom_rect(aes(xmin = 0.42, xmax = 0.58, ymin = 1.65, ymax = 1.68),
            fill = "#2C3E50", color = "#2C3E50") +
  theme_void()

# Create the sticker
sticker(
  p,
  package = "sensortowerR",
  p_size = 20,
  p_color = "#2C3E50",
  p_family = "roboto",
  p_fontface = "bold",
  h_color = "#2C3E50",
  h_fill = "#ECF0F1",
  url = "github.com/peterparkerspicklepatch/",
  u_size = 4,
  u_color = "#2C3E50",
  u_family = "roboto",
  filename = "images/sensortowerR_sticker.png",
  s_x = 1,
  s_y = 0.75,
  s_width = 0.5,
  spotlight = TRUE,
  l_x = 1,
  l_y = 1.1,
  l_width = 1.3,
  l_alpha = 0.3,
  l_color = "#2C3E50"
)
