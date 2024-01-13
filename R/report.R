#' Automatically detect the table type and output table and plot
#'
#' For each column prefix:
#' - If it matches a single column, use tab_var_counts and plot_var_counts
#' - If it matches multiple columns, use tab_items_counts and plot_item_counts
#'
#' TODO: Check whether the matched items are present
#'
#' @param data A tibble
#' @param scopes A list of column prefixes, e.g. "UM" or "WB" (character).
#' @param col_group A tidyselect column holding groups to compare, e.g. sd_gender (column name without quotes)
#' @param prop The basis of percent calculation when comparing groups: total (the default), cols, or rows
#' @param numbers The numbers plotted on bars: p (the default), n, both or NULL.
#' @param missings Include missing values (default FALSE)
#' @param ordered Values can be nominal (0) or ordered ascending (1) descending (-1).
#'                By default (NULL), the ordering is automatically detected.
#'                An appropriate color scale should be choosen depending on the ordering.
#'                For unordered values, the default scale is used.
#'                For ordered values, the viridis scale is used.
#' @export
report <- function(data, scopes, col_group=NULL, prop="total", numbers="p", missings=F, ordered=NULL) {

  chunks <- list()

  # Get item label from the attributes
  labels <- data %>%
    get_labels()

  for (scope in scopes) {

       # Get column candidates
       items <- labels %>%
         dplyr::filter(stringr::str_starts(item_name, scope)) %>%
         dplyr::distinct(item_group,item_name, item_class, item_label)

       # Only keep dominant item type
       if (nrow(items) > 1) {
         item_types <- items %>%
           dplyr::count(item_class,sort=T) %>%
           dplyr::slice_head(n=1)

         items <- dplyr::semi_join(items, item_types, by=c("item_class"))
       }


       is_items <- nrow(items) > 1
       is_var <- !is_items &&  (scope %in% colnames(data))
       is_scale <- get_scale(data, tidyselect::starts_with(scope), F)

       # A single categorical variable
       if (is_var && is_scale == 0 && is.null(col_group)) {

         chunks <- tab_var_counts(data, !!rlang::sym(scope)) %>%
           add_to_report(chunks)

         chunks <- plot_var_counts(data, !!rlang::sym(scope), numbers=numbers) %>%
           add_to_report(chunks)
       }

       # A single metric variable
       else if (is_var && is_scale != 0 && is.null(col_group)) {
         chunks <- tab_var_metrics(data, !!rlang::sym(scope)) %>%
           add_to_report(chunks)

         chunks <- plot_var_metrics(data, !!rlang::sym(scope)) %>%
           add_to_report(chunks)
       }

       # A single categorical variable by group
       else if (is_var && is_scale == 0 && !is.null(col_group)) {
         chunks <- tab_group_counts(data, !!rlang::sym(scope), !!rlang::sym(col_group), prop=prop, missings=missings) %>%
           add_to_report(chunks)

         chunks <- plot_group_counts(data, !!rlang::sym(scope), !!rlang::sym(col_group), prop=prop, numbers=numbers, ordered=ordered, missings=missings) %>%
           add_to_report(chunks)
       }

       # A single metric variable by group
       else if (is_var && is_scale != 0 && !is.null(col_group)) {
         chunks <- tab_group_metrics(data, !!rlang::sym(scope), !!rlang::sym(col_group)) %>%
           add_to_report(chunks)

         chunks <- plot_group_metrics(data, !!rlang::sym(scope), !!rlang::sym(col_group)) %>%
           add_to_report(chunks)
       }

       # Multiple items
       else if (is_items && is.null(col_group)) {

         chunks <- tab_item_counts(data, tidyselect::starts_with(scope)) %>%
           add_to_report(chunks)

         chunks <- plot_item_counts(data, tidyselect::starts_with(scope), numbers=numbers, ordered=ordered) %>%
           add_to_report(chunks)

       }

       # Multiple items by group
       else if (is_items && !is.null(col_group)) {

         chunks <- tab_multi_means(data, tidyselect::starts_with(scope), !!rlang::sym(col_group)) %>%
           add_to_report(chunks)

         chunks <- plot_multi_means(data, tidyselect::starts_with(scope), !!rlang::sym(col_group)) %>%
           add_to_report(chunks)

       }
       else {
         warning("Could't find columns to autodetect the table type for the scope ", scope,". Check your parameters.")
       }


       # Output index
       if (is_items && is_scale != 0) {
         #cat("**Index for ", scope, "**  \n")

         idx <- add_idx(data, tidyselect::starts_with(scope))
         idx_name <- setdiff(colnames(idx), colnames(data))


         if (is.null(col_group)) {
           chunks <- idx %>%
             tab_var_metrics(!!rlang::sym(idx_name)) %>%
             add_to_report(chunks)

           chunks <- idx %>%
             plot_var_metrics(!!rlang::sym(idx_name)) %>%
             add_to_report(chunks)
         }
         else {
           chunks <- idx %>%
             tab_group_metrics(!!rlang::sym(idx_name), !!rlang::sym(col_group)) %>%
             add_to_report(chunks)

           chunks <- idx %>%
             plot_group_metrics(!!rlang::sym(idx_name), !!rlang::sym(col_group)) %>%
             add_to_report(chunks)

         }
       }
  }

  class(chunks) <- c("vlkr_rprt", class(chunks))
  chunks
}

#' Add an object to the report list or print it
#'
#' @param obj A volker table or plot
#' @param chunks The current report list
#' @return A list with volker tables or plots
add_to_report <- function(obj, chunks) {
  if (knitr::is_html_output()) {

    if (inherits(obj,"vlkr_tbl")) {


      newchunk <-  knit_table(obj)
    }
    else if (inherits(obj,"vlkr_plt")) {
      newchunk <-  knit_plot(obj)
    } else {
      warning("Could not determine the volker report chunk type")
    }
    chunks <- append(chunks, newchunk)
  } else {
    print(obj)
  }
  chunks
}



#' Printing method for volker reports.
#'
#' @param obj The volker report
#' @export
print.vlkr_rprt <- function(obj) {
  if (knitr::is_html_output()) {
    obj %>%
      unlist() %>%
      paste0(collapse="\n")  %>%
      knitr::asis_output() %>%
      knitr::knit_print()
  }
}
