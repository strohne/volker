## code to prepare `chatgpt` dataset
chatgpt <- readr::read_rds("data-raw/chatgpt.rds")
usethis::use_data(chatgpt, overwrite = TRUE)
