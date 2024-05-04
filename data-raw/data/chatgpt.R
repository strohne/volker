#
#  Code to prepare `chatgpt` dataset
#

library(tidyverse)
library(volker)

#chatgpt <- readr::read_rds("data-raw/chatgpt.rds")

# Rename vars
# chatgpt <- chatgpt |>
#   rename(
#     use_private = use_private,
#     use_work = use_work,
#     adopter = adopter,
#     sd_gender = sd_gender,
#     sd_age = sd_age
#   )
#
# Add label to sd_age
# chatgpt <- labs_apply(
#   chatgpt,
#   tibble::tribble(
#     ~item_name, ~item_value,
#     "sd_age", "Age",
#     "adopter", "Innovator type"
#   )
# )

# Translate dataset
#write_csv2(chatgpt,"data-raw/chatgpt_de.csv", na="")

# Translate codebook
#codes_de <- codebook(chatgpt)
#write_csv2(codes_de,"data-raw/codes_de.csv", na="")

chatgpt_en <- read_csv2("data-raw/data/chatgpt_en.csv", na="")
codes_en <- read_csv2("data-raw/data/codes_en.csv", na="",col_types = "c")
chatgpt_en <- labs_apply(chatgpt_en, codes_en)

# Remove nonascii characters
chatgpt_en <- chatgpt_en |>
  mutate(cg_activities = str_replace_all(
    cg_activities,
    setNames(
      c("ae","ue","oe","Ae","Oe","Ue","ss"),
      c("ä","ü","ö","Ä","Ö","Ü","ß")
    )
  ))

nonascii <- chatgpt_en |>
  select(cg_activities) |>
  mutate(chars = str_extract_all(cg_activities,"[^a-zA-Z0-9\",;\\.<> /\\\\\\?\\(\\)-]")) |>
  filter(sapply(chars,\(x) length(x) > 0))

stopifnot(nrow(nonascii) == 0)

# Use
# chatgpt <- chatgpt_en
# usethis::use_data(chatgpt, overwrite = TRUE)
