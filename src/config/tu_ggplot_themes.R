
# tu_theme_standard -------------------------------------------------------

theme_tu_standard <- function(hex_col, hex_text = "#ffffff") {
  theme(strip.background = element_rect(fill = hex_col),
        strip.text = element_text(colour = hex_text, size = 10),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 16, color = hex_col),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 10)
  )
  
}
