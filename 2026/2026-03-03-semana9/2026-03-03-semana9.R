# Massa × Comprimento da Carapaça de Tartarugas Mediterrânicas | #TidyTuesday
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-03-03/readme.md

# Pacotes
library(ggplot2)
library(dplyr)
library(scales)

# Importação dos dados
tortoise_body_condition_cleaned <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-03/tortoise_body_condition_cleaned.csv')

df <- tortoise_body_condition_cleaned

# Minha paleta de cores
marrom_cafe  <- "#6B4F4F"
azul_base    <- "#224573"
marrom_claro <- "#A07878"
azul_claro   <- "#3D6FA8"
creme        <- "#F4EDE4"
grid_col     <- "#D9CECC"
texto_col    <- "#1E1414"
sub_col      <- "#5A3F3F"
cap_col      <- "#8C6B6B"

bci_cmap <- c("#6B4F4F", "#8C6B6B", "#B5A0A0", "#7A8FAB", "#3D6FA8", "#224573")

# Visualização
p <- df |>
  mutate(sex_label = ifelse(sex == "m", "♂ Macho", "♀ Fêmea")) |>
  ggplot(aes(
    x     = straight_carapace_length_mm,
    y     = body_mass_grams,
    color = body_condition_index,
    shape = sex_label)) +
  geom_point(alpha = 0.60, size = 2.2, stroke = 0.2) +
  scale_color_gradientn(colours = bci_cmap, name = "BCI") +
  scale_shape_manual(values = c("♂ Macho" = 16, "♀ Fêmea" = 17), name = "Sexo") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Massa × Comprimento da Carapaça.",
    subtitle = "Colorido pelo Índice de Condição Corporal.",
    x        = "Comprimento Reto da Carapaça (mm)",
    y        = "Massa Corporal (g)",
    caption  = "#TidyTuesday | Jennifer Lopes.") +
  theme_classic(base_family = "serif") +
  theme(
    plot.background   = element_rect(fill = creme, color = NA),
    panel.background  = element_rect(fill = creme, color = NA),
    text              = element_text(color = texto_col),
    plot.title        = element_text(size = 20, face = "bold", margin = margin(b = 6)),
    plot.subtitle     = element_text(size = 13, color = sub_col, margin = margin(b = 8)),
    plot.caption      = element_text(size = 10, color = cap_col),
    axis.title        = element_text(size = 13),
    axis.text         = element_text(size = 12, color = sub_col),
    axis.line         = element_line(color = grid_col),
    axis.ticks        = element_line(color = grid_col),
    legend.background = element_rect(fill = creme, color = NA),
    legend.key        = element_rect(fill = creme, color = NA),
    legend.title      = element_text(size = 13),
    legend.text       = element_text(size = 12),
    plot.margin       = margin(20, 24, 16, 20))

ggsave("2026-03-03-semana9.png", p, width = 12, height = 8, dpi = 300, bg = creme)