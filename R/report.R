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
#' @export
report <- function(data, scopes, col_group=NULL, prop="total") {

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

         items <- semi_join(items, item_types, by=c("item_class"))
       }


       is_items <- nrow(items) > 1
       is_var <- !is_items &&  (scope %in% colnames(data))
       is_scale <- get_scale(data, starts_with(scope))

       # A single categorical variable
       if (is_var && is_scale == 0 && is.null(col_group)) {
         tab_var_counts(data, !!rlang::sym(scope)) %>%
           print()

         plot_var_counts(data, !!rlang::sym(scope)) %>%
           print()
       }

       # A single metric variable
       else if (is_var && is_scale != 0 && is.null(col_group)) {
         tab_var_metrics(data, !!rlang::sym(scope)) %>%
           print()

         plot_var_metrics(data, !!rlang::sym(scope)) %>%
           print()
       }

       # A single categorical variable by group
       else if (is_var && is_scale == 0 && !is.null(col_group)) {
         tab_group_counts(data, !!rlang::sym(scope), !!rlang::sym(col_group), prop=prop) %>%
           print()

         plot_group_counts(data, !!rlang::sym(scope), !!rlang::sym(col_group), prop=prop) %>%
           print()
       }

       # A single metric variable by group
       else if (is_var && is_scale != 0 && !is.null(col_group)) {
         tab_group_metrics(data, !!rlang::sym(scope), !!rlang::sym(col_group)) %>%
           print()

         plot_group_metrics(data, !!rlang::sym(scope), !!rlang::sym(col_group)) %>%
           print()
       }

       # Multiple items
       else if (is_items && is.null(col_group)) {

         tab_item_counts(data, starts_with(scope)) %>%
           print()

         plot_item_counts(data, starts_with(scope)) %>%
           print()

       }

       # Multiple items by group
       else if (is_items && !is.null(col_group)) {

         tab_multi_means(data, starts_with(scope), !!rlang::sym(col_group)) %>%
           print()

         plot_multi_means(data, starts_with(scope), !!rlang::sym(col_group)) %>%
            print()

       }
       else {
         warning("Could't find columns to autodetect the table type for the scope ", scope,". Check your parameters.")
       }


       # Output index
       if (is_items && is_scale != 0) {
         idx <- add_idx(data, starts_with(scope))
         idx_name <- setdiff(colnames(idx), colnames(data))


         if (is.null(col_group)) {
           idx %>%
             tab_var_metrics(!!rlang::sym(idx_name)) %>%
             print()

           idx %>%
             plot_var_metrics(!!rlang::sym(idx_name)) %>%
             print()
         }
         else {
           idx %>%
             tab_group_metrics(!!rlang::sym(idx_name), !!rlang::sym(col_group)) %>%
             print()

           idx %>%
             plot_group_metrics(!!rlang::sym(idx_name), !!rlang::sym(col_group)) %>%
             print()

         }
       }

  }

#
#   class(result) <- c("vlkr_tbl", class(result))
#   result
}

