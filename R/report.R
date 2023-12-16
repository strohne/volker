#' Automatically detect the table type and output table and plot
#'
#' For each column prefix:
#' - If it matches a single column, use tab_var_counts and plot_var_counts
#' - If it matches multiple columns, use tab_items_counts and plot_item_counts
#'
#' @param data A tibble
#' @param scopes A list of column prefixes, e.g. "UM" or "WB.
#' @export
report <- function(data, scopes) {

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


       if ((nrow(items) == 1) &&  (scope %in% colnames(data))) {
         tab_var_counts(data, !!rlang::sym(scope)) %>%
           print()

         plot_var_counts(data, !!rlang::sym(scope)) %>%
           print()
       }
       else if (nrow(items) > 1) {
         tab_item_counts(data, starts_with(scope)) %>%
           print()

         plot_item_counts(data, starts_with(scope)) %>%
           print()

       }
       else {
         warning("Can't autodetect the table type for the scope ", scope)
       }
  }

#
#   class(result) <- c("vlkr_tbl", class(result))
#   result
}

