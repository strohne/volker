#
# Helper functions - metrics ----
#

#' Get term relevance per topic
#'
#' @param fit The topic model
#' @param lambda The weighting factor between prevalence and distinctiveness
#' @return A data frame with relevance metrics
lda_get_termrelevance <- function(fit, lambda=0.6) {

  lda_phi <- modeltools::posterior(fit)$terms %>% as.matrix()

  #' See http://vis.stanford.edu/files/2012-Termite-AVI.pdf and
  #' https://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf
  #lda_rel  = lambda * lda_phi + (1 - lambda) * (lda_phi / col_sums(lda_phi))

  # Calculate relevance
  # See section 3 in https://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf
  # relevance is p_kw for lambda = 1 and the probability shift for lambda = 0
  # mutate(relevance = lambda * log(p_kw) + (1- lambda) * log (p_kw / p_w))
  # TODO: is col_sums(lda_phi) correct here?
  lda_rel  = lambda * log(lda_phi) + (1 - lambda) * log(lda_phi / slam::col_sums(lda_phi))

  lda_rel <- lda_rel %>% t() %>% as_tibble(rownames ="term") %>%
    pivot_longer(-1, names_to="topic", values_to="relevance")

  lda_gini <- fit %>%
    tidytext::tidy(matrix = "beta") %>%
    group_by(term) %>%
    mutate(
      gini = DescTools::Gini(beta, unbiased=T),
      betasum = sum(beta)
    ) %>%
    ungroup()


  lda_gini %>%
    mutate(across(topic, as.character)) %>%
    left_join(lda_rel, by=c("topic", "term"))
}



lda_add_relevance <- function(data, fit, prefix="tpc_", lambda=0.6, seed=1852) {

  # Document relevance
  lda_theta <- modeltools::posterior(fit)$topics %>% as.matrix()
  lda_rel  = (lambda * log(lda_theta)) + (1 - lambda) * log(lda_theta / slam::col_sums(lda_theta))
  lda_rel <- lda_rel %>% as_tibble(rownames ="document") %>%
    pivot_longer(-1, names_to="topic", values_to="relevance")  %>%
    mutate(across(c(document, topic), as.character))  %>%
    group_by(topic) %>%
    arrange(desc(relevance)) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  # MDS
  lda_dist <- stats::dist(lda_theta)

  set.seed(seed)
  fit_mds <- cmdscale(lda_dist, k=2, list=T)
  lda_mds <- tibble(
    document = rownames(lda_theta),
    x = fit_mds$points[,1],
    y = fit_mds$points[,2],
  )

  # Gini
  lda_gini <- fit %>%
    tidytext::tidy(matrix = "gamma") %>%
    group_by(document) %>%
    mutate(
      gini = DescTools::Gini(gamma, unbiased=T)
      #gammasum = sum(gamma) -> always 1
    ) %>%
    ungroup()  %>%
    mutate(across(c(document, topic), as.character))

  # Join
  lda_gini <- lda_gini %>%
    dplyr::left_join(lda_rel, by=c("document","topic")) %>%
    dplyr::left_join(lda_mds, by=c("document"))

  lda_docs <- lda_gini %>%
    group_by(document) %>%
    arrange(desc(gamma)) %>%
    slice_head(n=1) %>%
    ungroup()

  # Add fit object to topic column
  attr(lda_docs$topic, "lda") <- fit

  # Prefix columns
  colnames(lda_docs) <- paste0(prefix, colnames(lda_docs))

  # Join and return
  data %>%
    mutate(.document = as.character(row_number())) %>%
    left_join(lda_docs, by =c(".document" = paste0(prefix, "document"))) %>%
    select(-.document)
}
