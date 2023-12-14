#' Output frequencies for multiple variables
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param values The values to print on the bars: n (frequency) or p (percentage)
#' @param .binary In case of boolean values, only one category is plotted. Set to TRUE to plot all categories.
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @export
plot_item_counts <- function(data, cols, values= c("n","p"), .binary=NULL, .labels=T) {


  result <- data %>%
      tab_item_counts(cols, values="n")

  values <- select(result,-matches("^Item|Total|Missing")) %>% colnames()


  pl <- result %>%
    tidyr::pivot_longer(-matches("^Item|Total|Missing"), names_to="value", values_to="n") %>%
    dplyr::mutate(p = (n / Total) * 100) %>%

    ggplot(aes(Item, y=p, fill=value)) +
    geom_col() +
    #scale_fill_manual(values=c("transparent", "black")) +
    #scale_y_reverse(labels=c("100%","75%","50%","25%","0%")) +
    scale_y_continuous(labels=c("0%","25%","50%","75%","100%")) +

    geom_text(aes(label=n),position=position_stack(vjust=0.5),size=3, color="white") +
    ylab("Share in percent") +
    coord_flip() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.title = element_blank()
    )

  if ((length(values) == 2) && (is.null(.binary) || (.binary))) {
    pl <- pl +
      scale_fill_manual(values=c("transparent", "black")) +
      theme(
        #legend.position="bottom",
        #legend.justification="left",
        legend.position="none"
      )
  }

  pl
}



#' Compare and plot factor value by group
#'
#' @param data A tibble containing data
#' @param col_category The column holding factor values
#' @param col_group The column holding groups to compare
#' @param relative Show absolute (F) or relative values (T)
#' @export
plot_compare_factor <- function(data, col_category, col_group, relative=F) {

  col_category <- enquo(col_category)
  col_group <- enquo(col_group)

  data <- data %>%
    count(!!col_group,!!col_category) %>%
    group_by(!!col_category) %>%
    mutate(p = n / sum(n)) %>%
    ungroup()

  if (relative) {
    col_value <- quo(p)
  } else {
    col_value <- quo(n)
  }

  data %>%
    mutate(!!col_category := forcats::fct_rev(!!col_category)) %>%
    ggplot(aes(!!col_category,y=!!col_value,fill=!!col_group)) +
    geom_col() +
    scale_fill_brewer(type="seq") +
    geom_text(aes(label=n),position=position_stack(vjust=0.5)) +

    coord_flip()

}



#' Compare and plot items
#' @param data A tibble containing item measures
#' @param cols_items Tidyselect item variables (e.g. starts_with...)
#' @param col_group Optional faceting variable
#' @importFrom dplyr filter
#' @export
plot_counts <- function(data, cols_items, col_group) {

  col_group <- enquo(col_group)

  # Get code labels from the attributes
  codes <- get_labels(data)

  # Filter item labels
  items <- codes %>%
    #na.omit() %>%
    distinct(item,label) %>%
    mutate(no = row_number())

  # Calculate
  data_grouped <- data %>%

    pivot_longer(all_of(cols_items), names_to="item",values_to="value_id") %>%

    count(!!col_group,item,value_id) %>%
    group_by(!!col_group,item) %>%
    mutate(p= n / sum(n)) %>%
    ungroup() %>%

    # Labeling
    mutate(value_id = as.character(value_id)) %>%
    left_join(items,by=c("item")) %>%
    left_join(select(codes,item,value_id,value),by=c("item","value_id")) %>%

    mutate(label = str_remove(label,"^.*: ")) %>%
    mutate(label = str_trunc(label,50) ) %>%

    mutate(item=str_remove(item,get_prefix(.$item))) %>%
    mutate(item=paste0(item," ",label)) %>%
    mutate(item=forcats::fct_reorder(item,no, .desc=T)) %>%

    mutate(value=paste0(value_id, " ", value))

  # Plot
  data_grouped %>%
    ggplot(aes(item,y=p,fill=value)) +

    geom_col() +
    geom_text(aes(label=n),position=position_stack(vjust=0.5),size=3) +

    scale_fill_brewer(type="qual") +
    scale_y_reverse(labels=c("100%","75%","50%","25%","0%")) +
    scale_x_discrete(position="top") +

    coord_flip()  +
    facet_wrap(vars(!!col_group)) +

    ylab("Anteil je Item in Prozent") +

    theme(
      legend.position="bottom",
      legend.justification="left",
      legend.title = element_blank(),
      axis.text.x=element_blank(),
      axis.title.y=element_blank()
    )
}
