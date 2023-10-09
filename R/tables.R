#' Output a frequency table for the values in one column
#'
#' @param data A tibble
#' @param col The column holding values to count
#' @param .quiet Set to true to suppress printing the output
#' @export
tab_var_counts <- function(data, col, .quiet = F) {

  result <- data %>%
    dplyr::count({{col}}) %>%
    dplyr::mutate(
      p = n / sum(n)
    ) %>%
    dplyr::mutate(
      valid = dplyr::if_else(
        is.na({{col}}),
        NA_real_,
        n / sum( na.omit(.)$n)
      )
    ) %>%
    dplyr::mutate(
      {{col}} := tidyr::replace_na(as.character({{col}}), "Fehlend")
    )

  result <- result %>%
    janitor::adorn_totals()

  if (!.quiet) {
    result %>%
      dplyr::mutate(
        p = paste0(round(p * 100,0),"%"),
        valid = ifelse(!is.na(valid), paste0(round(valid * 100,0),"%"), "-")
      ) %>%
      knitr::kable(align="lrrr") %>%
      print()
  }

  invisible(result)
}


#' Output frequencies cross tabulated with a grouping column
#'
#' @param data A tibble
#' @param col The column holding factor values
#' @param col_group The column holding groups to compare
#' @param values The values to output: n (frequency) or p (percentage).
#' @param .quiet Set to true to suppress printing the output
#' @export
tab_group_counts <- function(data, col, col_group, values=c("n","p"), .quiet=F) {

  grouped <- data %>%
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
      {{col_group}} := tidyr::replace_na(as.character({{col_group}}), "Fehlend"),
      {{col}} := tidyr::replace_na(as.character({{col}}), "Fehlend")
    )

  value <- "n"
  grouped_n <- grouped %>%
    select({{col_group}}, {{col}}, !!sym(value)) %>%
    pivot_wider(
      names_from = {{col_group}},
      values_from = !!sym(value),
      values_fill = setNames(list(0), value)
    )

  value <- "p"
  grouped_p <- grouped %>%
    select({{col_group}}, {{col}}, !!sym(value)) %>%
    pivot_wider(
      names_from = {{col_group}},
      values_from = !!sym(value),
      values_fill = setNames(list(0), value)
    )

  total <-  data %>%
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
      {{col}} := tidyr::replace_na(as.character({{col}}), "Fehlend")
    )

  value <- "n"
  total_n <- total %>%
    select({{col}}, Total = !!sym(value))

  value <- "p"
  total_p <- total %>%
    select({{col}}, Total = !!sym(value))

  result_n <-
    full_join(
      grouped_n,
      total_n,
      by = deparse(substitute(col))
    ) %>%
    janitor::adorn_totals()

  result_p <-
    full_join(
      grouped_p,
      total_p,
      by = deparse(substitute(col))
    ) %>%
    janitor::adorn_totals()

  result_p <- result_p %>%
    dplyr::mutate(across(where(is.numeric), ~ paste0(round(. * 100,0),"%" )))

  # Zip
  if (("n" %in% values) && ("p" %in% values)) {
    result <- zip_tables(result_n, result_p)
  }

  else if ("p" %in% values) {
    result <- result_p
  }

  else {
    result <- result_n
  }

  if (!.quiet) {

    result %>%
      knitr::kable(digits=0, align=c("l", rep("r",ncol(result) - 1))) %>%
      print()
  }

  invisible(result)
}


#' Output frequencies for multiple variables
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param values The values to output: n (frequency) or p (percentage)
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @param .quiet Set to true to suppress printing the output
#' @export
tab_item_counts <- function(data, cols, values= c("n","p"), .labels=T, .quiet=F) {

  # Calculate n and p
  result <- data %>%

    tidyr::pivot_longer(tidyselect::all_of(cols), names_to="item",values_to="value") %>%

    dplyr::count(item, value) %>%
    dplyr::group_by(item) %>%
    dplyr::mutate(p= n / sum(n)) %>%
    dplyr::ungroup()

  # Recode NA to "Fehlend"
  result <- result  %>%
    dplyr::mutate(
      value = factor(
        tidyr::replace_na(as.character(value), "Fehlend"),
        levels=c(unique(result$value), "Fehlend")
      )
    ) %>%
    arrange(value)

  # Absolute frequency
  value <- "n"
  result_n <- result %>%
    select(item, value, !!sym(value)) %>%
    pivot_wider(
      names_from = value,
      values_from = !!sym(value),
      values_fill = setNames(list(0), value)
    ) %>%
    janitor::adorn_totals("col")

  # Relative frequency
  value <- "p"
  result_p <- result %>%
    select(item, value, !!sym(value)) %>%
    pivot_wider(
      names_from = value,
      values_from = !!sym(value),
      values_fill = setNames(list(0), value)
    ) %>%
    janitor::adorn_totals("col")

  result_p <- result_p %>%
    dplyr::mutate(across(where(is.numeric), ~ paste0(round(. * 100,0),"%" )))

  if (("n" %in% values) && ("p" %in% values)) {
    result <- zip_tables(result_n, result_p)
  }

  else if ("p" %in% values) {
    result <- result_p
  }

  else {
    result <- result_n
  }

  # Get item labels from the attributes
  if (.labels) {
    codes <- data %>%
      dplyr::select(!!cols) %>%
      get_labels() %>%
      distinct(item, label)
  }

  if (.labels && (nrow(codes) > 0)) {
    result <- result %>%
      left_join(codes, by=c("item")) %>%
      mutate(item = dplyr::coalesce(label, item)) %>%
      select(-label)
  }

  # Remove common item prefix
  result <- result %>%
    dplyr::mutate(item = str_remove(item, get_prefix(.$item)))


  if (!.quiet) {
    result %>%
      knitr::kable(digits=0, align=c("l", rep("r",ncol(result) - 1))) %>%
      print()
  }

  invisible(result)
}


#' Output a five point summary table for the values in multiple columns
#'
#' @param data A tibble
#' @param col The columns holding metric values
#' @param digits The digits to print
#' @param .quiet Set to true to suppress printing the output
#' @export
tab_var_metrics <- function(data, col, digits=1, .quiet=F) {

  result <- data %>%
    skim_metrics({{col}}) %>%
    dplyr::select(
      "item" = skim_variable,
      min = numeric.min,
      q1 = numeric.q1,
      median = numeric.median,
      q3 = numeric.q3,
      max = numeric.max,
      m = numeric.mean,
      sd = numeric.sd,
      missing,
      n
    )

  result <- result %>%
    mutate(across(c(missing, n), ~ as.character(round(.,0)))) %>%
    mutate(across(c(min, q1, median, q3, max), ~ as.character(round(.,digits)))) %>%
    mutate(across(c(m, sd), ~ as.character(round(.,digits)))) %>%
    pivot_longer(-item) %>%
    dplyr::select(-item, {{col}} := name, value)


  if (!.quiet) {
      result %>%
        knitr::kable() %>%
        print()
  }

  invisible(result)
}

#' #' Output a five point summary for groups
#'
#' @param data A tibble
#' @param col The column holding metric values
#' @param col_group The column holding groups to compare
#' @param .quiet Set to true to suppress printing the output
#' @export
tab_group_metrics <- function(data, col, col_group, digits=1, .quiet=F) {

  result_grouped <- data %>%
    dplyr::group_by({{col_group}}) %>%
    skim_metrics({{col}}) %>%
    ungroup() %>%
    dplyr::mutate(
      {{col_group}} := tidyr::replace_na(as.character({{col_group}}), "Fehlend")
    ) %>%
    select(-skim_variable, -skim_type)

  result_total <-  data %>%
    skim_metrics({{col}}) %>%
    mutate({{col_group}} := "Total")

  result <- bind_rows(
    result_grouped,
    result_total
  ) %>%
    dplyr::select(
      {{col_group}},
      min = numeric.min,
      q1 = numeric.q1,
      median = numeric.median,
      q3 = numeric.q3,
      max = numeric.max,
      m = numeric.mean,
      sd = numeric.sd,
      missing,
      n
    )


  if (!.quiet) {

    result %>%
      knitr::kable(digits=digits) %>%
      print()
  }

  invisible(result)
}


#' Output a five point summary table for multiple items
#'
#' @param data A tibble
#' @param cols_values The columns holding metric values
#' @param digits The digits to print
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @param .quiet Set to true to suppress printing the output
#' @export
tab_item_metrics <- function(data, cols, digits=1, .labels=T, .quiet=F) {

  cols <- enquo(cols)

  result <- data %>%
    dplyr::select(!!cols) %>%
    skim_metrics()

  result <- result %>%
    dplyr::select(
      "item" = skim_variable,
      min = numeric.min,
      q1 = numeric.q1,
      median = numeric.median,
      q3 = numeric.q3,
      max = numeric.max,
      m = numeric.mean,
      sd = numeric.sd,
      missing,
      n
    )

  # Get item labels from the attributes
  if (.labels) {
    codes <- data %>%
      dplyr::select(!!cols) %>%
      get_labels() %>%
      distinct(item, label)
  }

  if (.labels && (nrow(codes) > 0)) {
    result <- result %>%
      left_join(codes, by=c("item")) %>%
      mutate(item = dplyr::coalesce(label, item)) %>%
      select(-label)
  }

  # Remove common item prefix
  result <- result %>%
    dplyr::mutate(item = str_remove(item, get_prefix(.$item)))

  if (!.quiet) {
    result %>%
      knitr::kable(digits=digits) %>%
      print()
  }

  invisible(result)
}



#' Output the means for groups in one or multiple columns
#'
#' TODO: handle completely missing data in single groups
#' TODO: zip mean and sd
#'
#' @param data A tibble
#' @param cols_values The columns that hold the values to summarize
#' @param cols_group The columns holding groups to compare
#' @param value The output metric, choosen from values of skim_metrics()
#' @param digits The digits to print
#' @param .quiet Set to true to suppress printing the output
#' @export
tab_item_group_metrics <- function(data, cols_values, cols_groups, value="numeric.mean", digits = 1, .quiet = F) {

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
    skim_metrics() %>%
    dplyr::select(skim_variable, all=!!sym(value))

  result <- map(
    cols,
    function(col) {
      col <- names(data)[col]

      data %>%
        dplyr::filter(!is.na(!!sym(col))) %>%
        dplyr::group_by(!!sym(col)) %>%
        dplyr::select(!!sym(col),!!vals) %>%
        skim_metrics() %>%
        dplyr::ungroup() %>%
        dplyr::select(skim_variable, !!sym(col), !!sym(value)) %>%
        tidyr::pivot_wider(
          names_from = !!sym(col),
          values_from = !!sym(value)
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
      left_join(codes, by=c("variable"="item")) %>%
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


#' Compare
#'
#' @param data A tibble containing item measures
#' @param cols_items Tidyselect item variables (e.g. starts_with...)
#' @param col_group Optional faceting variable
#' @export
tab_groups_items <- function(data, cols_items, col_group) {

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

    dplyr::mutate(item=str_remove(item,get_prefix(.$item))) %>%
    dplyr::mutate(item=paste0(item," ",label)) %>%
    dplyr::mutate(item=forcats::fct_reorder(item,no, .desc=T)) %>%

    dplyr::mutate(value=paste0(value_id, " ", value))

  data_grouped
}

