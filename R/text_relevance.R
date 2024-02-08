#
# Helper functions - metrics ----
#

#' Get term relevance per topic
#'
#' @keywords internal
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
  # dplyr::mutate(relevance = lambda * log(p_kw) + (1- lambda) * log (p_kw / p_w))
  # TODO: is col_sums(lda_phi) correct here?
  lda_rel  = lambda * log(lda_phi) + (1 - lambda) * log(lda_phi / slam::col_sums(lda_phi))

  lda_rel <- lda_rel %>% t() %>% tibble::as_tibble(rownames ="term") %>%
    tidyr::pivot_longer(-1, names_to="topic", values_to="relevance")

  lda_gini <- fit %>%
    tidytext::tidy(matrix = "beta") %>%
    dplyr::group_by(term) %>%
    dplyr::mutate(
      gini = gini(beta),
      betasum = sum(beta)
    ) %>%
    dplyr::ungroup()


  lda_gini %>%
    dplyr::mutate(dplyr::across(topic, as.character)) %>%
    dplyr::left_join(lda_rel, by=c("topic", "term"))
}


#' Add relevance
#'
#' @keywords internal
lda_add_relevance <- function(data, fit, prefix="tpc_", lambda=0.6, seed=1852) {

  # Document relevance
  lda_theta <- modeltools::posterior(fit)$topics %>% as.matrix()
  lda_rel  = (lambda * log(lda_theta)) + (1 - lambda) * log(lda_theta / slam::col_sums(lda_theta))
  lda_rel <- lda_rel %>% tibble::as_tibble(rownames ="document") %>%
    tidyr::pivot_longer(-1, names_to="topic", values_to="relevance")  %>%
    dplyr::mutate(dplyr::across(c(document, topic), as.character))  %>%
    dplyr::group_by(topic) %>%
    dplyr::arrange(dplyr::desc(relevance)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup()

  # MDS
  lda_dist <- stats::dist(lda_theta)

  set.seed(seed)
  fit_mds <- stats::cmdscale(lda_dist, k=2, list=T)
  lda_mds <- tibble::tibble(
    document = rownames(lda_theta),
    x = fit_mds$points[,1],
    y = fit_mds$points[,2],
  )

  # Gini
  lda_gini <- fit %>%
    tidytext::tidy(matrix = "gamma") %>%
    dplyr::group_by(document) %>%
    dplyr::mutate(
      gini = gini(gamma)
      #gammasum = sum(gamma) -> always 1
    ) %>%
    dplyr::ungroup()  %>%
    dplyr::mutate(dplyr::across(c(document, topic), as.character))

  # Join
  lda_gini <- lda_gini %>%
    dplyr::left_join(lda_rel, by=c("document","topic")) %>%
    dplyr::left_join(lda_mds, by=c("document"))

  lda_docs <- lda_gini %>%
    dplyr::group_by(document) %>%
    dplyr::arrange(dplyr::desc(gamma)) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup()

  # Add fit object to topic column
  attr(lda_docs$topic, "lda") <- fit

  # Prefix columns
  colnames(lda_docs) <- paste0(prefix, colnames(lda_docs))

  # Join and return
  data %>%
    dplyr::mutate(.document = as.character(dplyr::row_number())) %>%
    dplyr::left_join(lda_docs, by =c(".document" = paste0(prefix, "document"))) %>%
    dplyr::select(-tidyselect::all_of(".document"))
}
