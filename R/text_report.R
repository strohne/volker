#' Summarize a text column by topic modeling
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' @keywords internal
#'
#' @param data A data frame
#' @param col_text The text column
#' @param k The number of clusters / topics.
#'          If NULL, the number will be determined by metrics from the ldatuning package()
#' @param lambda The lambda value for relevance calculation of terms and example documents.
#'               The higher the lambda, the more specific terms come out.
#' @param seed Topic modeling, for finding k and for the final solution as well as
#'             multi dimensional scaling of the results are based on simulations.
#'             Fix the random number generator seed to a number for reproducible results.
#' @return A data frame with topic columns
#' @export
lda_report <- function(data, col_text, k=NULL, lambda=0.6, seed=1852) {

  lifecycle::signal_stage("experimental", "lda_report()")

  # TODO: add colname to prefix
  data <- lda_add_topic(data, {{col_text}}, prefix = "tpc_",k = k,lambda = lambda, seed = seed)

  # Extract lda object
  fit <- attr(data$tpc_topic,"lda")

  # LDA tuning result
  k <- fit@k_search
  if (!is.null(k[["metrics"]])) {
    ldatuning::FindTopicsNumber_plot(k[["metrics"]])
  }

  data <- data %>%
    dplyr::mutate(doc_length = stringr::str_length({{col_text}})) %>%
    volker::labs_apply(tibble::tibble("tpc_topic","Topic"))

  # cat("**Topic prevalence**  \n")
  # data %>%
  #   volker::plot_var_counts(tpc_topic) %>%
  #   print()

  cat("**Number of documents (n) and characters (m) per top topic**  \n")
  volker::tab_metrics_one_grouped(data, doc_length, tpc_topic) %>%
    print()

  # Top terms by relevance
  set.seed(seed)
  lda_plot_topterms(fit, lambda) %>%
    print()

  # MDS plot
  set.seed(seed)
  data %>%
    #slice_sample(n=1000) %>%
    dplyr::mutate(tpc_topdoc = tpc_rank <= 3) %>%
    lda_plot_docmds(tpc_x, tpc_y, tpc_topic, tpc_topdoc) %>%
    print()

  cat("  \n")
  data %>%
    lda_print_topdocs(tpc_topic, {{col_text}}, tpc_rank, 3)

  invisible(data)
}


#' Helper function
#'
#' @keywords internal
lda_plot_topterms <- function(fit, lambda = 0.6) {

  lda_get_termrelevance(fit, lambda) %>%

    # Get most relevant terms
    dplyr::group_by(topic) %>%
    dplyr::slice_max(relevance, n = 15)%>%
    dplyr::ungroup() %>%

    # Calculate stacks
    dplyr::mutate(betadiff = betasum - beta) %>%
    tidyr::pivot_longer(tidyselect::all_of(c("beta", "betadiff")), values_to="beta") %>%

    # Order values
    dplyr::mutate(
      topic = factor(topic),
      term = tidytext::reorder_within(term, relevance, topic),
      name = factor(name, levels=c("betadiff","beta"))
    ) %>%

    # Plot
    ggplot2::ggplot(ggplot2::aes(term, beta, fill = name)) +
    ggplot2::geom_col(alpha = 0.8, position  = "stack", show.legend = FALSE) +

    ggplot2::scale_fill_manual(values = c("gray", "blue")) +
    tidytext::scale_x_reordered() +
    ggplot2::facet_wrap(facets = ggplot2::vars(topic), scales = "free") + #, ncol = 3
    ggplot2::coord_flip() +
    ggplot2::ylab("beta & sum(beta)") +
    ggplot2::xlab("") +
    ggplot2::labs(
      title = "Top terms ordered by relevance",
      caption = paste0("Lamda=",lambda)
    ) +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.caption.position = "plot"
    )

}

#' Helper function
#'
#' @keywords internal
lda_plot_docmds <- function(data, col_x, col_y, col_topic, col_highlight=NULL) {

  # See lda_add_topic
  # lda_theta <- modeltools::posterior(fit)$topics %>% as.matrix
  #
  # lda_docs <- max.col(lda_theta)
  # lda_dist <- stats::dist(lda_theta)
  # fit_mds <- cmdscale(lda_dist, k=2, list=T)
  #
  # mds_loadings <- tibble::tibble(
  #     x = fit_mds$points[,1],
  #     y = fit_mds$points[,2],
  #     topic = lda_docs
  #   )

  data <- data %>%
    tidyr::drop_na({{col_topic}}) %>%
    dplyr::mutate({{col_topic}} := as.factor({{col_topic}}))

  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes(
      x={{col_x}},
      y={{col_y}},
      color={{col_topic}},
      fill={{col_topic}})
    ) +
    ggplot2::geom_text(ggplot2::aes(label={{col_topic}}),size=4, alpha=0.8)

  if (!missing(col_highlight)) {
    pl <- pl + ggplot2::geom_label(
      ggplot2::aes(
        label={{col_topic}}),
        color="white", size=4, alpha=0.8,
        data= dplyr::filter(data,{{col_highlight}})
    )
  }

  pl <- pl +
    ggplot2::ggtitle("MDS of documents according to their topic distribution dissimilarity") +
    ggplot2::labs(color="Topic", fill="Topic") +

    #    scale_x_log10()+
    #   scale_y_log10()+
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  pl
}

#' Helper function
#'
#' @keywords internal
lda_print_topdocs <- function(data, col_topic, col_text, col_rank, n=3, trunc=1000) {
  result <- c()

  topics <- stats::na.omit(unique(dplyr::pull(data, {{col_topic}})))
  topics <- topics[order(topics)]

  for (topic in topics) {

    examples <- data %>%
      dplyr::select({{col_topic}}, {{col_rank}}, {{col_text}}) %>%
      dplyr::filter({{col_topic}} == topic, {{col_rank}} <= n) %>%
      dplyr::pull({{col_text}})

    for (ex in examples) {
      result <- c(
        result,
        paste0("**Topic ", topic, "**  \n"),
        stringr::str_trunc(ex, trunc,side = "right"),
        "  \n\n"
      )
    }
  }

  result <- paste0(result, collapse="")
  cat(result)

}

