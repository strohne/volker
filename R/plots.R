
#' Plot the frequency of  values in one column
#'
#' @param data A tibble
#' @param col The column holding values to count
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @export
plot_var_counts <- function(data, col, .labels=T) {
  data %>%
    tab_var_counts({{col}}, .labels=.labels, .formatted=F) %>%
    dplyr::mutate(p = p * 100) %>%
    dplyr::rename(Item = 1) %>%
    dplyr::filter(! (Item %in% c("Total", "Missing"))) %>%

    # TODO: Make dry, see plot_item_counts and tab_group_counts
    ggplot(aes(Item, y=p)) +
      geom_col(fill="black") +
      scale_y_continuous(limits =c(0,100), labels=c("0%","25%","50%","75%","100%")) +

      geom_text(aes(label=n),position=position_stack(vjust=0.5),size=3, color="white") +
      ylab("Share in percent") +
      coord_flip() +
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank()
      )
}

#' Output frequencies for multiple variables
#'
#' @param data A tibble
#' @param col The column holding factor values
#' @param col_group The column holding groups to compare
#' @param values The values to print on the bars: n (frequency) or p (percentage).Not implemented yet.
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @param .binary Set a character value to focus only selected categories. In case of boolean values, automatically, only one category is plotted. Set to FALSE to plot all categories.
#' @export
plot_group_counts <- function(data, col, col_group, values=c("n","p"), .labels=T, .binary=NULL) {

  result <- data %>%
    tab_group_counts({{col}}, {{col_group}}, values="n",.labels = .labels, .formatted = F)

  values <- dplyr::select(result,-matches("^Item|Total|Missing")) %>% colnames()

  result <- result %>%
    rename(Item = 1) %>%
    tidyr::pivot_longer(-matches("^Item|Total|Missing"), names_to="value", values_to="n") %>%
    dplyr::mutate(p = (n / Total) * 100)

  if ((length(values) == 2) && (is.null(.binary))) {
    .binary <- "TRUE"
  }

  .plot_grouped_bars(result)

}

#' Output frequencies for multiple variables
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param values The values to print on the bars: n (frequency) or p (percentage). Not implemented yet.
#' @param .binary Set a character value to focus only selected categories. In case of boolean values, automatically, only one category is plotted. Set to FALSE to plot all categories.
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @export
plot_item_counts <- function(data, cols, values= c("n","p"), .labels=T, .binary=NULL) {


  result <- data %>%
      tab_item_counts(cols, values="n", .formatted=F)

  values <- dplyr::select(result,-matches("^Item|Total|Missing")) %>% colnames()


  result <- result %>%
    tidyr::pivot_longer(-matches("^Item|Total|Missing"), names_to="value", values_to="n") %>%
    dplyr::mutate(p = (n / Total) * 100)

  if ((length(values) == 2) && (is.null(.binary))) {
    .binary <- "TRUE"
  }

  .plot_grouped_bars(result)

}

#' Helper function: plot grouped bar chart
#'
#' @param data Dataframe with the columns Item, value, p, n
#' @param  .binary Category for filtering the dataframe
.plot_grouped_bars <- function(data, .binary=NULL) {

  if (!is.null(.binary)) {
    data <- filter(data, value == .binary)
  }

  pl <- data %>%

    ggplot(aes(Item, y=p, fill=value)) +
    geom_col() +
    #scale_fill_manual(values=c("transparent", "black")) +
    #scale_y_reverse(labels=c("100%","75%","50%","25%","0%")) +
    scale_y_continuous(limits =c(0,100), labels=c("0%","25%","50%","75%","100%")) +

    geom_text(aes(label=n),position=position_stack(vjust=0.5),size=3, color="white") +
    ylab("Share in percent") +
    coord_flip() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.title = element_blank()
    )

  #
  if (!is.null(.binary)) {
    pl <- pl +
      scale_fill_manual(values=c("black")) +
      theme(
        legend.position="bottom",
        legend.justification="left"
      )
  }

  pl
}
