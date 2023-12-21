
#' Plot the frequency of  values in one column.
#'
#' Note: only non-missing cases are used to calculate the percentage.
#'
#' @param data A tibble
#' @param col The column holding values to count
#' @param numbers The values to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @export
plot_var_counts <- function(data, col, numbers=NULL, .labels=T) {
  result <- data %>%
    tab_var_counts({{col}}, .labels=.labels, .formatted=F)

  # TODO: implement meta data property in tab_var_counts()
  title <- colnames(result)[1]
  base_n <- sum(result$n[! (result[[1]] %in% c("Total", "Missing"))])

  result <- result %>%
    dplyr::mutate(valid = valid * 100) %>%
    dplyr::rename(Item = 1) %>%
    dplyr::filter(! (Item %in% c("Total", "Missing")))  %>%
    dplyr::mutate(
      .values = case_when(
        all(numbers == "n") ~ as.character(n),
        all(numbers == "p") ~ paste0(round(valid,0), "%"),
        TRUE ~ paste0(n,"\n",round(valid,0), "%")
      )
    )

    # TODO: Make dry, see plot_item_counts and tab_group_counts
  pl <- result %>%
    ggplot(aes(Item, y=valid / 100)) +
      geom_col(fill="#611F53FF") +
      #scale_y_continuous(limits =c(0,100), labels=c("0%","25%","50%","75%","100%")) +
      scale_y_continuous(labels = scales::percent) +
      ylab("Share in percent") +
      coord_flip() +
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size=11),

        legend.title = element_blank(),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot"
      )

    if (!is.null(numbers)) {
      pl <- pl +
        geom_text(aes(label=.values),position=position_stack(vjust=0.5),size=3, color="white")
    }

  if (.labels) {
    pl <- pl + labs(title = title, caption = paste0("n=", base_n))
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
#' @param numbers The numbers to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param prop The basis of percent calculation: total (the default) or rows. To display column proportions, swap the first column and the grouping column.
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @param .category Set a character value to focus only selected categories. In case of boolean values, automatically, only one category is plotted. Set to FALSE to plot all categories.
#' @export
plot_group_counts <- function(data, col, col_group, numbers=NULL, prop="total", .labels=T, .category=NULL) {


  if (prop == "cols") {
    stop("To display column proportions, swap the first and the grouping column. Then set the prop parameter to \"rows\".")
  }

  result <- data %>%
    tab_group_counts({{col}}, {{col_group}}, values="n",.labels = .labels, .formatted = F)

  title <- colnames(result)[1]
  base_n <- sum(result$Total[! (result[[1]] %in% c("Total", "Missing"))])
  categories <- dplyr::select(result,-1,-matches("^Total|Missing")) %>% colnames()

  # Detect whether the categories are a numeric sequence and choose direction
  ordered <- suppressWarnings(as.numeric(c(categories)))
  positive <- ordered[ordered >= 0]
  direction <- dplyr::coalesce(ifelse(any(diff(positive) >= 0) | any(is.na(ordered)), -1 , 1), 1)


  # Detect whether the categories are binary
  if ((length(categories) == 2) && (is.null(.category)) && ("TRUE" %in% categories)) {
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
    dplyr::mutate(value = factor(value, levels= categories))

  if (prop == "rows") {
    result <- result %>%
      group_by(Item) %>%
      dplyr::mutate(p = (n / sum(n)) * 100) %>%
      ungroup()

  } else {
    result <- result %>%
      dplyr::mutate(p = (n / sum(n)) * 100)
  }
  result <- result %>%
    dplyr::mutate(
      .values = case_when(
        all(numbers == "n") ~ as.character(n),
        all(numbers == "p") ~ paste0(round(p, 0), "%"),
        TRUE ~ paste0(n,"\n",round(p,0), "%")
      )
    )


  plot_grouped_bars(
    result,
    category= .category,
    numbers=numbers,
    direction = direction,
    title = ifelse(.labels, title, NULL),
    caption = ifelse(.labels, paste0("n=", base_n), NULL)
  )

}

#' Output frequencies for multiple variables
#'
#' Note: only non-missing cases are used to calculate the percentage.
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param numbers The values to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param .category Set a character value to focus only selected categories. In case of boolean values, automatically, only one category is plotted. Set to FALSE to plot all categories.
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @export
plot_item_counts <- function(data, cols, numbers=NULL, .labels=T, .category=NULL) {


  result <- data %>%
    tab_item_counts(cols, values="n", .formatted=F)

  title <- colnames(result)[1]
  categories <- dplyr::select(result,-1,-matches("^Total|Missing")) %>% colnames()
  base_n <- sum(dplyr::select(result,-1,-matches("^Total|Missing"))[1,])

  # Detect whether the categories are a numeric sequence and choose direction
  ordered <- suppressWarnings(as.numeric(c(categories)))
  positive <- ordered[ordered >= 0]
  direction <- dplyr::coalesce(ifelse(any(diff(positive) >= 0) | any(is.na(ordered)), -1 , 1), 1)


   # Detect whether the categories are binary
   if ((length(categories) == 2) && (is.null(.category)) && ("TRUE" %in% categories)) {
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
    dplyr::mutate(value = factor(value, levels= categories)) %>%
    dplyr::group_by(Item) %>%
    dplyr::mutate(p = (n / sum(n)) * 100) %>%
    ungroup() %>%
    dplyr::mutate(
      .values = case_when(
        all(numbers == "n") ~ as.character(n),
        all(numbers == "p") ~ paste0(round(p,0), "%"),
        TRUE ~ paste0(n,"\n",round(p,0), "%")
      )
    )


  plot_grouped_bars(
    result,
    category=.category,
    numbers = numbers,
    direction = direction,
    title = ifelse(.labels, title, NULL),
    caption = ifelse(.labels, paste0("n=", base_n, "; multiple responses possible"), NULL)
  )

}


#' Output averages for multiple variables
#'
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param limits The scale limits. Set NULL to extract limits from the labels.
#' @param numbers The values to print on the bars: "m" or NULL
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @param .negative If False (default) negative values are recoded as missing values
#' @export
plot_item_metrics <- function(data, cols, limits=NULL, numbers=NULL, .labels=T, .negative=F) {

  result <- data %>%
    tab_item_metrics(cols, .labels=.labels, .negative=.negative)

  title <- colnames(result)[1]

  # TODO: minus missing values, output range
  base_n <- max(result$n)

  # TODO: set the scale

  pl <- result %>%
    dplyr::rename(Item=1) %>%

    ggplot(aes(Item, y=m)) +
      geom_col(fill="#611F53FF")

  if (is.null(limits)) {
    limits <- attr(result,"limits")
  }

  # Add scales, labels and theming
  pl <- pl +
      scale_y_continuous() +
      scale_x_discrete(labels = scales::label_wrap(40)) +
      ylab("Mean values") +
      coord_flip(ylim = limits) +
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size=11),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.caption.position = "plot"
      )


  if (!is.null(numbers)) {
    pl <- pl +
      geom_text(
        #aes(label=paste0("⌀", round(m,1))),
        aes(label=round(m,1)),
        position=position_stack(vjust=0.5),
        size=3,
        color="white"
      )
  }

  if (!is.null(.labels)) {
    pl <- pl + ggtitle(label = title)
  }


  if (!is.null(.labels)) {
    pl <- pl + labs (caption = paste0("n=", base_n))
  }

  pl
}


#' Helper function: plot grouped bar chart
#'
#' @param data Dataframe with the columns Item, value, p, n
#' @param category Category for filtering the dataframe
#' @param numbers The values to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param direction Direction of the viridis scale, either -1 or 1.
#' @param title The plot title or NULL
#' @param caption The plot caption or NULL. The caption is used for notes.
plot_grouped_bars <- function(data, category=NULL, numbers=NULL, direction=-1, title=NULL, caption=NULL) {

  if (!is.null(category)) {
    data <- filter(data, value == category)
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
    ylab("Share in percent") +
    coord_flip() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y = element_text(size=11),
      legend.title = element_blank(),
      plot.caption = element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )

  # Simplify binary plot
  if (!is.null(category)) {
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
        direction = direction
      )
  }

  if (!is.null(numbers)) {
    pl <- pl +
      geom_text(aes(label=.values),position=position_stack(vjust=0.5),size=3, color="white")
  }

  if (!is.null(title)) {
    pl <- pl + ggtitle(label = title)
  }


  if (!is.null(caption)) {
    pl <- pl + labs (caption = caption)
  }

  pl
}
