# VolkeR: Plots

library(tidyverse)
library(volker)

data <- volker::chatgpt

# Recode -9 to NA
data <- mutate(data, across(starts_with("cg_adoption_"), ~ na_if(.,-9)  ))

# Plots
plot_compare_factor(data, in_adoption, sd_geschlecht)
  # ggsave("plots/plot_compater_factor.png", width = 13, height = 6)

plot_counts(data, starts_with("cg_adoption_advantage"))
  # ggsave("plots/plot_counts2.png", width = 13, height = 6)



