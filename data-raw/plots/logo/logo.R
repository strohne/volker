#
# Create hex-sticker ----
#

library(tidyverse)
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
  h_fill="white",
  h_color="#006363",
  filename = "plots/logo_tmp.png"
)


plot(s)

usethis::use_logo("plots/logo_tmp.png", geometry = "240x278", retina = TRUE)

# Button
# GenerateSticker
sticker(
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
  h_size=2,
  h_fill="white",
  h_color="#006363",
  filename = "plots/button_tmp.png"
) %>% plot()

yellow <- "#D89900"
pink <- "#CD52A3"
magenta <- "#8F0B70"
dark_magenta <- "#611938"
red <- "#9E1910"

cs_purple <- "#0C0A69"
cs_beige <- "#F6EED2"
cs_beige2 <- "#F9F3DF"

v_blue1 <- "#96dfde"
v_blue2 <- "#008b8b"
v_blue3 <- "#006363"

green1 <- "#ACDEBA"
green2 <- "#74B976"
green3 <- "#488E4A"
# Different color schemes:
bar_plot <- ggplot(bar_data, aes(x=cat, y=val, fill=cat)) +
  geom_bar(stat="identity") +
  #scale_fill_manual(values = c("A" = "#96dfde", "B" = "#008b8b", "C" = "#006363", "D" = "black")) +
 # scale_fill_manual(values=c("A" = "#CD52A3", "B" = "#8F0B70", "C" = "black", "D" = "#D89900"))+
  scale_fill_manual(values=c("A" = "white", "B" = "white", "C" = "white", "D" = "white"))+
  theme_void() +
  theme(legend.position="none")

# GenerateSticker
s_magenta <- sticker(
  bar_plot,
  package = "volkeR",
  p_size = 15,
  #p_color = "#008b8b",
  p_color = "white",
  p_family="robo_mono",
  p_y = 0.53,
  s_x = 1,
  s_y = 1.1,
  s_width = 0.95,
  s_height = 0.95,
  h_fill="#9E1910",
  h_color="#9E1910",
  filename = "plots/logo_tmp.png"
)

plot(s_magenta)
