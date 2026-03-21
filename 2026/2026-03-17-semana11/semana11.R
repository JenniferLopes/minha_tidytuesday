# TidyTuesday 2026 · Week 11
# Mortalidade de Salmonídeos na Noruega (2020-2025)
# Fonte: Norwegian Veterinary Institute / Laksetap

library(tidyverse)
library(ragg)

# Paleta ----------------------------------------------
marrom_cafe <- "#6B4F4F"
azul_base   <- "#224573"
creme       <- "#F4EDE4"
grid_col    <- "#D9CECC"
texto_col   <- "#1E1414"
sub_col     <- "#5A3F3F"
cap_col     <- "#8C6B6B"

cores_tipo <- c(
  "Mortos"      = azul_base,
  "Descartados" = "#3D6FA8",
  "Fugidos"     = marrom_cafe,
  "Outros"      = "#A07878")

# Load data -------------------------------------------
tuesdata <- tidytuesdayR::tt_load("2026-03-17")
monthly_losses_data <- tuesdata$monthly_losses_data

# Preparo ---------------------------------------------
nacional <- monthly_losses_data |>
  filter(
    geo_group == "country",
    region    == "Norge",
    species   == "salmon") |>
  select(date, dead, discarded, escaped, other) |>
  pivot_longer(
    cols      = c(dead, discarded, escaped, other),
    names_to  = "tipo",
    values_to = "n") |>
  mutate(
    tipo = case_match(
      tipo,
      "dead"      ~ "Mortos",
      "discarded" ~ "Descartados",
      "escaped"   ~ "Fugidos",
      "other"     ~ "Outros"),
    tipo      = factor(tipo, levels = c("Mortos", "Descartados", "Fugidos", "Outros")),
    n_milhoes = n / 1e6)

pico <- nacional |>
  group_by(date) |>
  summarise(total = sum(n_milhoes), .groups = "drop") |>
  slice_max(total, n = 1)

# Tema ------------------------------------------------
theme_chaos <- theme_classic(base_family = "Georgia") +
  theme(
    plot.background    = element_rect(fill = creme,     color = NA),
    panel.background   = element_rect(fill = creme,     color = NA),
    panel.grid.major.x = element_line(color = grid_col, linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    text               = element_text(color = texto_col),
    plot.title         = element_text(size = 18, face = "bold",
                                      margin = margin(b = 6)),
    plot.subtitle      = element_text(size = 10, color = sub_col,
                                      lineheight = 1.5,
                                      margin = margin(b = 16)),
    plot.caption       = element_text(size = 8,  color = cap_col,
                                      hjust = 0,
                                      margin = margin(t = 12)),
    axis.title.x       = element_text(size = 10, color = sub_col,
                                      margin = margin(t = 8)),
    axis.title.y       = element_text(size = 10, color = sub_col,
                                      margin = margin(r = 8)),
    axis.text          = element_text(size = 9,  color = sub_col),
    axis.line          = element_line(color = grid_col),
    axis.ticks         = element_line(color = grid_col),
    legend.background  = element_rect(fill = creme, color = NA),
    legend.key         = element_rect(fill = creme, color = NA),
    legend.text        = element_text(size = 9,  color = sub_col),
    legend.title       = element_text(size = 9,  color = sub_col),
    legend.position    = "top",
    legend.justification = "left",
    plot.margin        = margin(24, 28, 16, 20))

# Plot ------------------------------------------------
p <- ggplot(nacional, aes(x = date, y = n_milhoes, fill = tipo)) +
  
  geom_area(
    position = position_stack(reverse = FALSE),
    alpha    = 0.88) +
  
  annotate(
    "text",
    x      = pico$date,
    y      = pico$total + 0.4,
    label  = paste0("Pico: ", round(pico$total, 1), "M perdas\n",
                    format(pico$date, "%b/%Y")),
    family = "Georgia",
    size   = 3,
    color  = sub_col,
    hjust  = 0.5) +
  
  scale_fill_manual(
    values = cores_tipo,
    name   = "Tipo de perda") +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y",
    expand      = c(0.01, 0)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "M"),
    expand = c(0.01, 0)) +
  
  labs(
    title    = "O que acontece com os salmões noruegueses?",
    subtitle = paste(
      "Total mensal de perdas na aquicultura da Noruega entre 2020 e 2025.",
      "A maior parte das perdas é composta por mortes dentro das fazendas.",
      "Fugas e descartes representam uma parcela menor, mas constante.",
      sep = "\n"),
    x       = NULL,
    y       = "Indivíduos perdidos (milhões)",
    caption = paste(
      "TidyTuesday 2026 · Week 11",
      "Fonte: Norwegian Veterinary Institute / apps.vetinst.no/laksetap",
      "Jennifer Lopes.",
      sep = "  |  ")) +
  theme_chaos

p

# Save ------------------------------------------------
ggsave(
  filename = "tidytuesday_2026_w11_salmonidos.png",
  plot     = p,
  width    = 12,
  height   = 7,
  dpi      = 300,
  bg       = creme,
  device   = ragg::agg_png)
