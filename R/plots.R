
#' Plot the frequency of  values in one column.
#'
#' Note: only non-missing cases are used to calculate the percentage.
#'
#' @param data A tibble
#' @param col The column holding values to count
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @export
plot_var_counts <- function(data, col, .labels=T) {
  result <- data %>%
    tab_var_counts({{col}}, .labels=.labels, .formatted=F)

  caption <- colnames(result)[1]

  result <- result %>%
    dplyr::mutate(valid = valid * 100) %>%
    dplyr::rename(Item = 1) %>%
    dplyr::filter(! (Item %in% c("Total", "Missing")))


    # TODO: Make dry, see plot_item_counts and tab_group_counts
  pl <- result %>%
    ggplot(aes(Item, y=valid / 100)) +
      geom_col(fill="#611F53FF") +
      #scale_y_continuous(limits =c(0,100), labels=c("0%","25%","50%","75%","100%")) +
      scale_y_continuous(labels = scales::percent) +

      geom_text(aes(label=n),position=position_stack(vjust=0.5),size=3, color="white") +
      ylab("Share in percent") +
      coord_flip() +
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(size=11)
      )

  if (.labels) {
    pl <- pl + ggtitle(label = caption)
  }

  pl
}

#' Plot frequencies cross tabulated with a grouping column
#'
#' Note: only non-missing cases are used to calculate the percentage.
#'
#' @param data A tibble
#' @param col The column holding factor values
#' @param col_group The column holding groups to compare
#' @param values The values to print on the bars: n (frequency) or p (percentage).Not implemented yet.
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @param .category Set a character value to focus only selected categories. In case of boolean values, automatically, only one category is plotted. Set to FALSE to plot all categories.
#' @export
plot_group_counts <- function(data, col, col_group, values=c("n","p"), .labels=T, .category=NULL) {

  result <- data %>%
    tab_group_counts({{col}}, {{col_group}}, values="n",.labels = .labels, .formatted = F)

  caption <- colnames(result)[1]
  values <- dplyr::select(result,-1,-matches("^Total|Missing")) %>% colnames()

  # Detect whether the values are a numeric sequence and choose direction
  .ordered <- suppressWarnings(as.numeric(c(values)))
  .direction <- dplyr::coalesce(ifelse(all(diff(.ordered) >= 0) | any(!is.na(.ordered)), -1 , 1), 1)

  # Detect whether the values are binary
  if ((length(values) == 2) && (is.null(.category)) && ("TRUE" %in% values)) {
    .category <- "TRUE"
  }

  result <- result %>%
    rename(Item = 1) %>%
    dplyr::filter(! (Item %in% c("Total", "Missing"))) %>%
    dplyr::select(-matches("^Total|Missing")) %>%
    tidyr::pivot_longer(
      -Item,
      names_to="value",
      values_to="n",
    ) %>%
    dplyr::mutate(value = factor(value, levels= values)) %>%
    #group_by(Item) %>%
    dplyr::mutate(p = (n / sum(n)) * 100)# %>%
    #ungroup()



  plot_grouped_bars(
    result,
    .category=.category,
    .direction = .direction,
    caption = ifelse(.labels, caption, NULL)
  )

}

#' Output frequencies for multiple variables
#'
#' Note: only non-missing cases are used to calculate the percentage.
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param values The values to print on the bars: n (frequency) or p (percentage). Not implemented yet.
#' @param .category Set a character value to focus only selected categories. In case of boolean values, automatically, only one category is plotted. Set to FALSE to plot all categories.
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @export
plot_item_counts <- function(data, cols, values= c("n","p"), .labels=T, .category=NULL) {


  result <- data %>%
    tab_item_counts(cols, values="n", .formatted=F)

  caption <- colnames(result)[1]
  values <- dplyr::select(result,-1,-matches("^Total|Missing")) %>% colnames()

  # Detect whether the values are a numeric sequence and choose direction
  .ordered <- suppressWarnings(as.numeric(c(values)))
  .direction <- dplyr::coalesce(ifelse(all(diff(.ordered) >= 0) | any(!is.na(.ordered)), -1 , 1), 1)


   # Detect whether the values are binary
   if ((length(values) == 2) && (is.null(.category)) && ("TRUE" %in% values)) {
     .category <- "TRUE"
   }

  result <- result %>%
    dplyr::rename(Item=1) %>%
    dplyr::select(-matches("^Total|Missing$"))%>%
    tidyr::pivot_longer(
      -Item,
      names_to="value",
      values_to="n"
    ) %>%
    dplyr::mutate(value = factor(value, levels= values)) %>%
    dplyr::group_by(Item) %>%
    dplyr::mutate(p = (n / sum(n)) * 100) %>%
    ungroup()


  plot_grouped_bars(
    result,
    .category=.category, .direction = .direction,
    caption = ifelse(.labels, caption, NULL)
  )

}

#' Helper function: plot grouped bar chart
#'
#' @param data Dataframe with the columns Item, value, p, n
#' @param caption The plot caption or NULL
#' @param  .category Category for filtering the dataframe
#' @param .direction Direction of the viridis scale, either -1 or 1.
plot_grouped_bars <- function(data, caption=NULL, .category=NULL, .direction=-1) {

  if (!is.null(.category)) {
    data <- filter(data, value == .category)
  }

  pl <- data %>%

    ggplot(aes(Item, y=p / 100, fill=value)) +
    geom_col() +
    #scale_fill_manual(values=c("transparent", "black")) +
    #scale_y_reverse(labels=c("100%","75%","50%","25%","0%")) +

    # Add 0.1 to avoid "Removed 1 rows containing missing values (`geom_col()`)."
    #scale_y_continuous(limits =c(0,100.1), labels=c("0%","25%","50%","75%","100%")) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = scales::label_wrap(40)) +
    geom_text(aes(label=n),position=position_stack(vjust=0.5),size=3, color="white") +
    ylab("Share in percent") +
    coord_flip() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.title = element_blank(),
      axis.text.y = element_text(size=11)
    )

  # Simplify binary plot
  if (!is.null(.category)) {
    pl <- pl +
      scale_fill_manual(values=c("#611F53FF")) +
      theme(
        legend.position="bottom",
        legend.justification="left"
      )
  } else {
    pl <- pl +
      viridis::scale_fill_viridis(
        discrete=TRUE,
        option="rocket",
        direction = .direction
      )
  }

  if (!is.null(caption)) {
    pl <- pl + ggtitle(label = caption)
  }

  pl
}
