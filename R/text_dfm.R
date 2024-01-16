#
# Helper functions - Text preparation ----
#

#' Get cleaned tokens
#'
#' @param msg A data frame with the column "doc" holding a document identifier
#'            and "text" holding the text
#' @return A quanteda tokens object
get_tokens <- function(msg) {
  # Mentions, Tags, Entities, URLs, erstes Wort in Klammern entfernen
  msg <- msg %>%
    dplyr::mutate(text = str_remove_all(text, "@[^ ]+")) %>%
    dplyr::mutate(text = str_remove_all(text, "\\<[^\\>]+\\>")) %>%
    dplyr::mutate(text=str_remove_all(text,'https?://[^\\s"<>]+')) %>%
    dplyr::mutate(text = str_remove_all(text, "&[a-z]+;")) %>%
    dplyr::mutate(text = str_remove_all(text, "^\\([^\\)]{0,3}\\)"))

  # Tokenize
  msg_words <- tokenizers::tokenize_words(msg$text)
  names(msg_words)  <- msg$doc
  msg_tokens <- quanteda::tokens(msg_words)

  # Kleinschreibung
  msg_tokens <- msg_tokens %>%
    quanteda::tokens_tolower()

  # Interpunktion entfernen
  msg_tokens <- msg_tokens %>%
    quanteda::tokens_remove(pattern = "[[:punct:][:blank:]]+", valuetype = "regex")

  # Leerzeichen entfernen
  msg_tokens <- msg_tokens %>%
    quanteda::tokens_remove(pattern = "[[:space:]]+", valuetype = "regex") %>%
    quanteda::tokens_remove(pattern = "\u2800", valuetype = "regex")

  # Stopwords EN/DE entfernen
  msg_tokens <- msg_tokens %>%
    quanteda::tokens_remove(pattern=stopwords::stopwords("en", source = "stopwords-iso")) %>%
    quanteda::tokens_remove(pattern=stopwords::stopwords("de", source = "stopwords-iso"))

  # Zahlen entfernen
  msg_tokens <- msg_tokens %>%
    quanteda::tokens_remove(pattern = "[0-9]+", valuetype = "regex")

  # Tokens mit weniger als 2 Zeichen entfernen
  msg_tokens <- msg_tokens %>%
    quanteda::tokens_remove(pattern = "^.{1,2}$", valuetype = "regex")

  # Leere Token entfernen
  msg_tokens <- msg_tokens %>%
    quanteda::tokens_remove("")

  msg_tokens
}

#' Get a pruned dfm from tokens
#'
#' @param msg_tokens A quanteda tokens object
#' @return A quanteda dfm
get_dfm <- function(msg_tokens) {

  # Convert to dfm
  msg_dfm  <- quanteda::dfm(msg_tokens)

  # Remove tokens occuring in less than 5 documents
  msg_dfm <- quanteda::dfm_trim(msg_dfm,min_docfreq = 5, docfreq_type = "count")

  # Remove tokens occuring in more than 99% of the documents
  #msg_dfm <- quanteda::dfm_trim(msg_dfm,max_docfreq = 0.99, docfreq_type = "quantile")

  # Remove empty documents
  msg_dfm <-  quanteda::dfm_subset(msg_dfm, quanteda::ntoken(msg_dfm) > 0, drop_docid = FALSE)

  msg_dfm
}
