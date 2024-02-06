## code to prepare `chatgpt` dataset
chatgpt <- readr::read_rds("data-raw/chatgpt.rds")

# Add label to sd_alter
chatgpt <- labs_apply(
  chatgpt,
  tibble::tribble(
    ~item_name, ~item_value,
    "sd_alter", "Alter",
    "in_adoption", "Innovationstyp"
  )
)

# Use
usethis::use_data(chatgpt, overwrite = TRUE)
