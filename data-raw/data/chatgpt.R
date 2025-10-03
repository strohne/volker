#
#  Code to prepare `chatgpt` dataset
#

library(tidyverse)
library(volker)

chatgpt_en <- read_csv2("data-raw/data/chatgpt_en.csv", na="")

# Set labels and categories
chatgpt_en$sd_gender <- factor(chatgpt_en$sd_gender, levels=c("female","male","diverse","[no answer]"))
chatgpt_en$adopter <- factor(chatgpt_en$adopter, levels = c(
  "I try new offers immediately",
  "I try new offers rather quickly",
  "I wait until offers establish themselves",
  "I only use new offers when I have no other choice",
  "[no answer]"
))


codes_en <- read_csv2("data-raw/data/codes_en.csv", na="",col_types = "c")
chatgpt_en <- labs_apply(chatgpt_en, codes_en)

# Remove nonascii characters
chatgpt_en <- chatgpt_en |>
  labs_store() |>
  mutate(cg_activities = str_replace_all(
    cg_activities,
    setNames(
      c("ae","ue","oe","Ae","Oe","Ue","ss"),
      c("ä","ü","ö","Ä","Ö","Ü","ß")
    )
  )) |>
  labs_restore()

nonascii <- chatgpt_en |>
  select(cg_activities) |>
  mutate(chars = str_extract_all(cg_activities,"[^a-zA-Z0-9\'\",;\\.<> /\\\\\\?\\(\\)-]")) |>
  filter(sapply(chars,\(x) length(x) > 0))

stopifnot(nrow(nonascii) == 0)

# Use (uncomment to update the data set)
#chatgpt <- chatgpt_en
#usethis::use_data(chatgpt, overwrite = TRUE)
