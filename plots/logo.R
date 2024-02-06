#
# Create hex-sticker ----
#

library(hexSticker)
library(ggplot2)
library(showtext)

# Add google font
font_add_google("Roboto Mono", "robo_mono")

# Data for bar chart
bar_data <- tibble(
  cat = c("A", "B", "C", "D"),
  val = c(4, 7, 6, 9)
)

# Bar chart
bar_plot <- ggplot(bar_data, aes(x=cat, y=val, fill=cat)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("A" = "#96dfde", "B" = "#008b8b", "C" = "#006363", "D" = "black")) +
  theme_void() +
  theme(legend.position="none")

# GenerateSticker
s <- sticker(
  bar_plot,
  package = "volkeR",
  p_size = 15,
  #p_color = "#008b8b",
  p_color = "black",
  p_family="robo_mono",
  p_y = 0.53,
  s_x = 1,
  s_y = 1.1,
  s_width = 0.95,
  s_height = 0.95,
  h_fill="transparent",
  h_color="#006363",
  filename = "logo.png"
)


plot(s)

# usethis::use_logo("plots/logo.png", geometry = "240x278", retina = TRUE)
