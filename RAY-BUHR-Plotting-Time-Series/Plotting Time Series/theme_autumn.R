color_palette_autumn <- c("#37220c", "#be8b0a", "#bbddbe", "#797e89", "#415674")

autumn_pal <- function() { scales::manual_pal(color_palette_autumn) }

scale_color_autumn <- function(...) discrete_scale("colour", "autumn", autumn_pal(), ...)

scale_fill_autumn <- function(...) discrete_scale("fill", "autumn", autumn_pal(), ...)

theme_autumn <- function() {
  theme_minimal(base_size = 14, base_family = "Roboto") %+replace%
    theme(legend.position = "top")
}
