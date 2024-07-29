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


#
# Logo ----
#


# Bar chart
bar_plot <- ggplot(bar_data, aes(x=cat, y=val, fill=cat)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("A" = "#96dfde", "B" = "#008b8b", "C" = "#006363", "D" = "black")) +
  theme_void() +
  theme(legend.position="none")

# Sticker
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

#
# Button ----
#

# Bar plot
bar_plot <- ggplot(bar_data, aes(x=cat, y=val, fill=cat)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("A" = "#96dfde", "B" = "#008b8b", "C" = "#006363", "D" = "black")) +
  theme_void() +
  theme(legend.position="none")

# Generate sticker
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
  filename = "data-raw/plots/logo/button.png"
) %>% plot()

#
# Color variations ---
#

v_blue1 <- "#96dfde"
v_blue2 <- "#008b8b"
v_blue3 <- "#006363"

green1 <- "#ACDEBA"
green2 <- "#74B976"
green3 <- "#488E4A"

pink <- "#CD52A3"
magenta <- "#8F0B70"
dark_magenta <- "#611938"

yellow <- "#D89900"

# White bar plot ----
bar_plot <- ggplot(bar_data, aes(x=cat, y=val, fill=cat)) +
  geom_bar(stat="identity") +
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
  h_fill=v_blue2,
  h_color=v_blue2,
  # h_fill="#9E1910",
  # h_color="#9E1910",
  filename = "data-raw/plots/logo/volker_white.png"
) |> plot()


# green bar plot ----
bar_plot <- ggplot(bar_data, aes(x=cat, y=val, fill=cat)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("A" = green1, "B" = green2, "C" = magenta, "D" = green3))+
  theme_void() +
  theme(legend.position="none")

# GenerateSticker
s_magenta <- sticker(
  bar_plot,
  package = "volkeR",
  p_size = 15,
  p_color = magenta,
  p_family="robo_mono",
  p_y = 0.53,
  s_x = 1,
  s_y = 1.1,
  s_width = 0.95,
  s_height = 0.95,
  h_fill="white",
  h_color=magenta,
  filename = "data-raw/plots/logo/volker_green.png"
) |> plot()


# magenta bar plot ----
bar_plot <- ggplot(bar_data, aes(x=cat, y=val, fill=cat)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("A" = pink, "B" = magenta, "C" = "black", "D" = dark_magenta))+
  theme_void() +
  theme(legend.position="none")

# GenerateSticker
s_magenta <- sticker(
  bar_plot,
  package = "volkeR",
  p_size = 15,
  p_color = dark_magenta,
  p_family="robo_mono",
  p_y = 0.53,
  s_x = 1,
  s_y = 1.1,
  s_width = 0.95,
  s_height = 0.95,
  h_fill="white",
  h_color=dark_magenta,
  filename = "data-raw/plots/logo/volker_magenta.png"
) |> plot()


# purplegreen bar plot ----
bar_plot <- ggplot(bar_data, aes(x=cat, y=val, fill=cat)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("A" = pink, "B" = magenta, "C" = v_blue3, "D" = v_blue2))+
  theme_void() +
  theme(legend.position="none")

# GenerateSticker
s_magenta <- sticker(
  bar_plot,
  package = "volkeR",
  p_size = 15,
  p_color = magenta,
  p_family="robo_mono",
  p_y = 0.53,
  s_x = 1,
  s_y = 1.1,
  s_width = 0.95,
  s_height = 0.95,
  h_fill="white",
  h_color=magenta,
  filename = "data-raw/plots/logo/volker_purplegreen.png"
) |> plot()

# wildmix bar plot ----
bar_plot <- ggplot(bar_data, aes(x=cat, y=val, fill=cat)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("A" = pink, "B" = magenta, "C" = v_blue3, "D" = yellow))+
  theme_void() +
  theme(legend.position="none")

# GenerateSticker
s_magenta <- sticker(
  bar_plot,
  package = "volkeR",
  p_size = 15,
  p_color = v_blue3,
  p_family="robo_mono",
  p_y = 0.53,
  s_x = 1,
  s_y = 1.1,
  s_width = 0.95,
  s_height = 0.95,
  h_fill="white",
  h_color=v_blue3,
  filename = "data-raw/plots/logo/volker_wildmix.png"
) |> plot()

