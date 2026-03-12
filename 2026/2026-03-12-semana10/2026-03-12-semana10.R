# TidyTuesday 2026 · Week 10
# CAPphrase: The Scale of Chaos
# Source: Kucharski AJ (2026) DOI: 10.5281/zenodo.18750055

library(tidyverse)
library(showtext)
library(ragg)

# Fonts -----------------------------------------------
font_add_google("Libre Baskerville", "baskerville")
showtext_auto()
showtext_opts(dpi = 300)

# Paleta ----------------------------------------------
marrom_cafe <- "#6B4F4F"
azul_base   <- "#224573"
creme       <- "#F4EDE4"
grid_col    <- "#D9CECC"
texto_col   <- "#1E1414"
sub_col     <- "#5A3F3F"
cap_col     <- "#8C6B6B"

# Load data -------------------------------------------
tuesdata <- tidytuesdayR::tt_load("2026-03-10")

absolute_judgements <- tuesdata$absolute_judgements

# Summary ---------------------------------------------
phrase_summary <- absolute_judgements |>
  group_by(term) |>
  summarise(
    n        = n(),
    median   = median(probability, na.rm = TRUE),
    q10      = quantile(probability, 0.10, na.rm = TRUE),
    q25      = quantile(probability, 0.25, na.rm = TRUE),
    q75      = quantile(probability, 0.75, na.rm = TRUE),
    q90      = quantile(probability, 0.90, na.rm = TRUE),
    variance = var(probability, na.rm = TRUE),
    .groups  = "drop") |>
  mutate(
    var_norm = (variance - min(variance)) / (max(variance) - min(variance)),
    term_ord = fct_reorder(term, median))

# Official benchmarks ---------------------------------
benchmarks <- tribble(
  ~label,                  ~value,
  "Almost certain",            95,
  "Highly likely",             85,
  "Likely",                    70,
  "About even",                50,
  "Unlikely",                  30,
  "Very unlikely",             15,
  "Exceptionally unlikely",     5)

# Tema ------------------------------------------------
theme_chaos <- theme_classic(base_family = "baskerville") +
  theme(
    plot.background    = element_rect(fill = creme,      color = NA),
    panel.background   = element_rect(fill = creme,      color = NA),
    panel.grid.major.x = element_line(color = grid_col,  linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    text               = element_text(color = texto_col),
    plot.title         = element_text(size = 22, face = "bold",
                                      margin = margin(b = 6)),
    plot.subtitle      = element_text(size = 9,  color = sub_col,
                                      lineheight = 1.6,
                                      margin = margin(b = 20)),
    plot.caption       = element_text(size = 8,  color = cap_col,
                                      hjust = 0,
                                      margin = margin(t = 14)),
    axis.title.x       = element_text(size = 10, color = sub_col,
                                      margin = margin(t = 8)),
    axis.title.y       = element_blank(),
    axis.text          = element_text(size = 8,  color = sub_col),
    axis.line          = element_line(color = grid_col),
    axis.ticks         = element_line(color = grid_col),
    legend.background  = element_rect(fill = creme, color = NA),
    legend.key         = element_rect(fill = creme, color = NA),
    legend.text        = element_text(size = 8,  color = sub_col),
    legend.title       = element_text(size = 8,  color = sub_col),
    legend.position    = "top",
    legend.justification = "left",
    plot.margin        = margin(24, 28, 16, 20))

# Plot ------------------------------------------------
p1 <- ggplot(phrase_summary) +
  
  geom_vline(
    data      = benchmarks,
    aes(xintercept = value),
    colour    = grid_col,
    linewidth = 0.4,
    linetype  = "dashed") +
  
  geom_segment(
    aes(x = q10, xend = q90,
        y = term_ord, yend = term_ord,
        colour = var_norm),
    linewidth = 4,
    alpha     = 0.22,
    lineend   = "round") +
  
  geom_segment(
    aes(x = q25, xend = q75,
        y = term_ord, yend = term_ord,
        colour = var_norm),
    linewidth = 4,
    lineend   = "round") +
  
  geom_point(
    aes(x = median, y = term_ord),
    colour = texto_col,
    size   = 2,
    shape  = 21,
    fill   = creme,
    stroke = 1.1) +
  
  scale_colour_gradient(
    low   = azul_base,
    high  = marrom_cafe,
    name  = "Variance (normalised, low to high)",
    guide = guide_colourbar(
      barwidth       = 10,
      barheight      = 0.4,
      title.position = "top",
      ticks          = FALSE)) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"),
    expand = c(0.01, 0)) +
  
  labs(
    title    = "The Scale of Chaos",
    subtitle = paste(
      "How much does a probability phrase actually mean?",
      "Each bar shows how 5,000+ respondents translated a term into a number.",
      "Bar = IQR (25th-75th percentile).  Extension = 10th-90th.  Dot = median.",
      "Colour encodes variance: blue = consensus, brown = disagreement.",
      "Dashed lines mark official benchmarks (UK PHIA / IPCC).",
      sep = "\n"),
    x       = "Probability estimate (%)",
    caption = paste(
      "TidyTuesday 2026 · Week 10",
      "Source: Kucharski AJ (2026) CAPphrase. DOI: 10.5281/zenodo.18750055",
      "Jennifer Lopes.",
      sep = "  |  ")) +
  theme_chaos

# Save ------------------------------------------------
ggsave(
  filename = "2026-03-12-semana10.png",
  plot     = p1,
  width    = 10,
  height   = 8,
  dpi      = 300,
  bg       = creme,
  device   = ragg::agg_png)