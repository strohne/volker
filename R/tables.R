
#' Output a frequency table for the values in one column
#'
#' @param data A tibble
#' @param col The column holding values to count
#' @param .quiet Set to true to suppress printing the output
#' @export
tab_counts <- function(data, col, .quiet = F) {

  result <- data %>%
    dplyr::count({{col}}) %>%
    dplyr::mutate(
      p = n / sum(n)
    ) %>%
    dplyr::mutate(
      p_val = dplyr::if_else(
        is.na({{col}}),
        NA_real_,
        n / sum( na.omit(.)$n)
      )
    ) %>%
    dplyr::mutate(
      {{col}} := tidyr::replace_na(as.character({{col}}),"Fehlend")
    )

  result <- result %>%
    janitor::adorn_totals()

  if (!.quiet) {
    result %>%
      dplyr::mutate(
        p = round(p * 100,0),
        p_val = round(p_val * 100,0)
      ) %>%
      dplyr::rename(
        `%` = p,
        `% (gültig)` = p_val
      ) %>%
      knitr::kable() %>%
      print()
  }

  invisible(result)
}


#' Count and compare groups (cross table)
#'
#' @param data A tibble
#' @param col The column holding factor values
#' @param col_group The column holding groups to compare
#' @param value The value to output: n (frequency) or p (percentage)
#' @param digits The digits to print
#' @param .quiet Set to true to suppress printing the output
#' @export
tab_group_counts <- function(data, col, col_group, value="n", digits = 1, .quiet=F) {

  result_grouped <- data %>%
    dplyr::count({{col}}, {{col_group}}) %>%
    dplyr::group_by({{col_group}}) %>%
    dplyr::mutate(
      p = n / sum(n)
    ) %>%
    # dplyr::mutate(
    #   p_val = dplyr::if_else(
    #     is.na({{col}}),
    #     NA_real_,
    #     n / sum( na.omit(.)$n)
    #   )
    # ) %>%
    ungroup() %>%
    dplyr::mutate(
      {{col_group}} := tidyr::replace_na(as.character({{col_group}}),"Fehlend"),
      {{col}} := tidyr::replace_na(as.character({{col}}),"Fehlend")
    )

  result_grouped <- result_grouped %>%
    #pivot_wider(names_from = {{col_group}}, values_from = c(n,p), values_fill=list("n"=0,"p"=0))
    pivot_wider(names_from = {{col_group}}, values_from = value, values_fill=list(value=0))


  result_total <-  data %>%
    dplyr::count({{col}}) %>%
    dplyr::mutate(
      p = n / sum(n, na.rm = T)
    ) %>%
    # dplyr::mutate(
    #   p_val = dplyr::if_else(
    #     is.na({{col}}),
    #     NA_real_,
    #     n / sum( na.omit(.)$n)
    #   )
    # ) %>%
    dplyr::mutate(
      {{col}} := tidyr::replace_na(as.character({{col}}),"Fehlend")
    ) %>%
    select({{col}}, value)

  result <- full_join(result_grouped, result_total)

  result <- result %>%
    janitor::adorn_totals()


  if (!.quiet) {
    result %>%
      knitr::kable(digits=digits) %>%
      print()
  }

  invisible(result)
}

#' Compare & Table items
#'
#' @param data A tibble containing item measures
#' @param cols_items Tidyselect item variables (e.g. starts_with...)
#' @param col_group Optional faceting variable
#' @export
tab_compare_items <- function(data, cols_items, col_group) {

  col_group <- enquo(col_group)

  # Get code labels from the attributes
  codes <- tibble(
    item = colnames(data),
    label = sapply(data,attr,"comment"),
    value = lapply(data,attributes)
  ) %>%
    dplyr::mutate(label=as.character(label)) %>%
    tidyr::unnest_longer(value) %>%
    dplyr::filter(value_id != "comment", value_id != "class" ) %>%
    dplyr::mutate(value = as.character(value))

  # Filter item labels
  items <- codes %>%
    #na.omit() %>%
    dplyr::distinct(item,label) %>%
    dplyr::mutate(no = row_number())

  # Calculate
  data_grouped <- data %>%

    tidyr::pivot_longer(tidyselect::all_of(cols_items), names_to="item",values_to="value_id") %>%

    dplyr::count(!!col_group,item,value_id) %>%
    dplyr::group_by(!!col_group,item) %>%
    dplyr::mutate(p= n / sum(n)) %>%
    dplyr::ungroup() %>%

    # Labeling
    dplyr::mutate(value_id = as.character(value_id)) %>%
    dplyr::left_join(items,by=c("item")) %>%
    dplyr::left_join(select(codes,item,value_id,value),by=c("item","value_id")) %>%

    dplyr::mutate(label = str_remove(label,"^.*: ")) %>%
    dplyr::mutate(label = str_trunc(label,50) ) %>%

    dplyr::mutate(item=str_remove(item,common_prefix(.$item))) %>%
    dplyr::mutate(item=paste0(item," ",label)) %>%
    dplyr::mutate(item=forcats::fct_reorder(item,no, .desc=T)) %>%

    dplyr::mutate(value=paste0(value_id, " ", value))

  data_grouped
}





#' Output a five point summary table for the values in one or multiple columns
#'
#' @param data A tibble
#' @param cols_values The columns holding metric values
#' @param digits The digits to print
#' @param .quiet Set to true to suppress printing the output
#' @export
tab_metrics <- function(data, cols_values, digits = 1, .quiet = F) {

  cols_values <- enquo(cols_values)

  result <- data %>%
    dplyr::select(!!cols_values) %>%
    skim_metrics()

  if (!.quiet) {
    result %>%
      dplyr::select(
        " " = skim_variable,
        Min = numeric.min,
        Q1 = numeric.q1,
        Median = numeric.median,
        Q3 = numeric.q3,
        Max = numeric.max,
        n,
        missing
      ) %>%
      knitr::kable(digits=digits) %>%
      print()
  }

  invisible(result)
}


#' Output the means for groups in one or multiple columns
#'
#' @param data A tibble
#' @param cols_group The columns holding groups to compare
#' @param cols_values The columns that hold the values to calculate the mean
#' @param digits The digits to print
#' @param .quiet Set to true to suppress printing the output
#' @export
tab_means <- function(data, cols_groups, cols_values, digits = 1, .quiet = F) {

  # Get positions of group cols
  cols <- tidyselect::eval_select(
    expr = enquo(cols_groups),
    data = data[unique(names(data))],
    allow_rename = FALSE,
    error_call = rlang::error_call
  )

  vals <- enquo(cols_values)

  all <- data %>%
    dplyr::select(!!vals) %>%
    skimr::skim_without_charts() %>%
    dplyr::select(skim_variable, all=numeric.mean)

  result <- map(
    cols,
    function(col) {
      col <- names(data)[col]

      data %>%
        dplyr::filter(!is.na(!!sym(col))) %>%
        dplyr::group_by(!!sym(col)) %>%
        dplyr::select(!!sym(col),!!vals) %>%
        skimr::skim_without_charts() %>%
        dplyr::ungroup() %>%
        dplyr::select(skim_variable, !!sym(col), numeric.mean) %>%
        tidyr::pivot_wider(
          names_from = !!sym(col),
          values_from = numeric.mean
        )
    }
  ) %>%
    purrr::reduce(
      dplyr::inner_join,
      by="skim_variable"
    )

  result <- dplyr::inner_join(all, result, by="skim_variable") %>%
    dplyr::rename(variable=skim_variable)


  codes <- data %>%
    dplyr::select(!!vals) %>%
    get_labels() %>%
    distinct(item, label)

  if (nrow(codes) > 0) {
    result <- result %>%
      left_join(codes,by=c("variable"="item")) %>%
      mutate(variable = ifelse(is.na(label), variable, label)) %>%
      select(-label)
  }


  if (!.quiet) {
    result %>%
      knitr::kable(digits=digits) %>%
      print()
  }

  invisible(result)
}
