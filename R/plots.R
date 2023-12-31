
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
    pl <- pl + labs(title = title, caption = paste0("n=", base_n,"; without missings"))
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

  # Check columns
  has_column(data, {{col}})
  has_column(data, {{col_group}})

  if (prop == "cols") {
    stop("To display column proportions, swap the first and the grouping column. Then set the prop parameter to \"rows\".")
  }

  result <- data %>%
    tab_group_counts({{col}}, {{col_group}}, values="n",.labels = .labels, .formatted = F)

  title <- colnames(result)[1]
  base_n <- sum(result$Total[! (result[[1]] %in% c("Total", "Missing"))])
  categories <- dplyr::select(result,-1,-matches("^Total|Missing")) %>% colnames()

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

  .plot_grouped_bars(
    result,
    category= .category,
    scale = get_scale(data, {{col}}, categories),
    numbers=numbers,
    title = ifelse(.labels, title, NULL),
    caption = ifelse(.labels, paste0("n=", base_n,"; without missings."), NULL)
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

  .plot_grouped_bars(
    result,
    category=.category,
    scale = get_scale(data, cols, categories),
    numbers = numbers,
    title = ifelse(.labels, title, NULL),
    caption = ifelse(.labels, paste0("n=", base_n, "; multiple responses possible, without missings."), NULL)
  )

}

#' Output a histogram for a single metric variable
#'
#' @param data A tibble
#' @param col The columns holding metric values
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @export
plot_var_metrics <- function(data, col, .labels=T) {

  data <- drop_na(data,{{col}})

  # TODO: make configurable: density, boxplot or histogram
  pl <- data %>%
    ggplot(aes({{col}})) +
    geom_histogram(fill="#611F53FF", bins=20)
    #geom_density(fill="#611F53FF")


  if (.labels) {
    title <- get_col_label(data,{{col}})
    base_n <- data %>%  nrow()
    pl <- pl +
      labs(title = title, caption = paste0("n=", base_n, "; without missings")) +
      xlab(title)
  }

  pl
}

#' Output averages for multiple variables
#'
#'
#' @param data A tibble containing item measures
#' @param col The column holding metric values
#' @param col_group The column holding groups to compare
#' @param limits The scale limits. Set NULL to extract limits from the labels.
#' @param numbers The values to print on the bars: "m" or NULL
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @param .negative If False (default) negative values are recoded as missing values
#' @export
plot_group_metrics <- function(data, col, col_group, limits=NULL, numbers=NULL, .labels=T, .negative=F) {

  result <- tab_group_metrics(data, {{col}}, {{col_group}}, .labels=.labels, .negative=.negative)

  # Pimp the result
  result <- result[result[[1]] != "Total", ]
  colnames(result)[1] <- get_col_label(data, {{col}})

  .plot_means(result, limits, numbers, .labels)
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
  data %>%
    tab_item_metrics(cols, .labels=.labels, .negative=.negative) %>%
    .plot_means(limits, numbers, .labels)
}


#' Output averages for multiple variables compared by a grouping variable
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param cols_group The columns holding groups to compare
#' @param limits The scale limits. Set NULL to extract limits from the labels.
#' @param numbers The values to print on the bars: "m" or NULL
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @param .negative If False (default) negative values are recoded as missing values
#' @return A plot
#' @export
plot_multi_means <- function(data, cols, cols_groups, limits=NULL, numbers=NULL, .labels=T, .negative=F) {

  # Get positions of group cols
  cols_groups <- tidyselect::eval_select(expr = enquo(cols_groups), data=data)
  cols <- enquo(cols)

  # TODO: warn if any negative values were recoded
  # TODO: only selected columns
  if (!.negative) {
    data <- dplyr::mutate(data, across(where(is.numeric), ~ if_else(. < 0, NA, .)))
  }


  # Grouped means
  result <- map(
    cols_groups,
    function(col) {
      col <- names(data)[col]

      data %>%
        dplyr::filter(!is.na(!!sym(col))) %>%
        dplyr::group_by(!!sym(col)) %>%
        dplyr::select(!!sym(col),!!cols) %>%
        skim_metrics() %>%
        dplyr::ungroup() %>%
        dplyr::select(item=skim_variable, group=!!sym(col), numeric.mean)
    }
  ) %>%
    purrr::reduce(
      dplyr::bind_rows
    )

  # TODO: Set limits
  # if (is.null(limits)) {
  #   limits <- attr(result,"limits")
  # }

  # Replace item labels
  if (.labels) {
    result <- replace_item_values(result, data, cols)
  }

  # Remove common item prefix
  prefix <- get_prefix(result$item)
  if (prefix != "") {
    result <- dplyr::mutate(result, item = str_remove(item, prefix))
    result <- dplyr::mutate(result, item = ifelse(item=="", prefix, item))
  }

  #print(result)
  #class(result) <- setdiff(class(result),"skim_df")

  pl <- result %>%
    ggplot(aes(item, y=numeric.mean, fill=group)) +
    geom_col(position = "dodge")

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

  if (!is.null(.labels)) {
    # TODO: remove missing values, get group sizes
    base_n <- nrow(data)
    pl <- pl + ggtitle(label =trim_label(prefix))
    pl <- pl + labs (caption = paste0("n=", base_n, "; with missings"))
  }

  pl
}

#' Helper function: plot grouped bar chart
#'
#' @param data Dataframe with the columns Item, value, p, n
#' @param category Category for filtering the dataframe
#' @param scale Direction of the scale: 0 = no direction for categories,
#'              -1 = descending or 1 = ascending values.
#' @param numbers The values to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param title The plot title or NULL
#' @param caption The plot caption or NULL. The caption is used for notes.
.plot_grouped_bars <- function(data, category=NULL, scale=NULL, numbers=NULL, title=NULL, caption=NULL) {

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
  } else if ((scale > 0) || (scale < 0)) {
    pl <- pl +
      viridis::scale_fill_viridis(
        discrete=TRUE,
        option="rocket",
        direction = scale
      )
  } else {
    pl <- pl +
      ggplot2::scale_fill_discrete()
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

#' Helper function to plot means as bars, e.g. for plot_item_metrics and plot_group_metrics
#'
#' @param result The result table of tab_item_metrics() or tab_group_metrics()
#' @param limits The scale limits. Set NULL to extract limits from the labels.
#' @param numbers The values to print on the bars: "m" or NULL
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @return Plot
.plot_means <- function(result, limits, numbers, .labels) {


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
    pl <- pl + labs (caption = paste0("n=", base_n))
  }

  pl
}
