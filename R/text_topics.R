#
# Helper functions - Modeling ----
#

lda_find_k <- function(dfm, sample.size=2000, plot=T) {
  metrics <- dfm %>%
    quanteda::dfm_sample(min(quanteda::ndoc(.), sample.size)) %>%
    ldatuning::FindTopicsNumber(
      topics = c(c(2:9),seq(from = 10, to = 30, by = 5)),
      #metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      metrics = c("Griffiths2004", "CaoJuan2009"),
      control = list(seed = 77),
      mc.cores = 7L,
      verbose = TRUE
    )

  if (plot) {
    ldatuning::FindTopicsNumber_plot(metrics)
  }

  k <- mean(
    metrics$topics[which.max(metrics$Griffiths2004)],
    #metrics$topics[which.max(metrics$Deveaud2014)],
    #metrics$topics[which.min(metrics$Arun2010)],
    metrics$topics[which.min(metrics$CaoJuan2009)]
  )

  list(
    k = k,
    metrics = metrics
  )
}

#' Perform topic modeling on a text column and add the relevance scores of each topic to each row
#'
#' @param data A tibble
#' @param col_text The column containing text
#' @param prefix A prefix added to the new topic columns
#' @param k The number of clusters / topics.
#'          If NULL, the number will be determined by metrics from the ldatuning package()
#' @param lambda The lambda value for relevance calculation of terms and example documents.
#'               The higher the lambda, the more specific terms come out.
#' @param seed A seed for the random number generator, makes results reproducible. You should change it!
#' @param plot Whether to plot ldatuning results
#' @return A data frame with a new column for each topic
#' @export
lda_add_topic <- function(data, col_text, prefix="tpc_", k=NULL, lambda=0.6, seed=1852, plot=F) {

  # Tokenize and convert to dfm
  ds_texts <- data %>%
    dplyr::mutate(doc = dplyr::row_number())
    dplyr::select(doc, text={{col_text}})
    tidyr::drop_na()

  ds_tokens <- get_tokens(ds_texts)
  ds_dfm <- get_dfm(ds_tokens)

  # Find k
  if (is.null(k)) {
    k_search <- lda_find_k(ds_dfm, plot=plot)
    k <- k_search$k
  } else {
    k_search <- list(k=k)
  }

  # Fit model
  fit <- topicmodels::LDA(ds_dfm, k=k_search$k , control = list(seed = seed))

  # TODO: Not the best hack in case the LDA_VEM class is changed in future versions.
  #       Better set k_search as an attribute of the topic column.
  slot(fit, "k_search", check=F) <- k_search

  # Calculate and add output values
  lda_add_relevance(data, fit, prefix, lambda, seed=seed)
}
