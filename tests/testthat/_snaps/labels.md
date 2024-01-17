# Labels

    Code
      volker::get_labels(data)
    Output
      # A tibble: 91 x 9
         item_group item_class item_name item_label value_name value_label item_prefix
         <chr>      <chr>      <chr>     <chr>      <chr>      <chr>       <chr>      
       1 cg         <NA>       cg_nutze~ ChatGPT-N~ 1          nie         ""         
       2 cg         <NA>       cg_nutze~ ChatGPT-N~ 2          seltener    ""         
       3 cg         <NA>       cg_nutze~ ChatGPT-N~ 3          mehrmals p~ ""         
       4 cg         <NA>       cg_nutze~ ChatGPT-N~ 4          mehrmals p~ ""         
       5 cg         <NA>       cg_nutze~ ChatGPT-N~ 5          fast tägli~ ""         
       6 cg         <NA>       cg_nutze~ ChatGPT-N~ 1          nie         ""         
       7 cg         <NA>       cg_nutze~ ChatGPT-N~ 2          seltener    ""         
       8 cg         <NA>       cg_nutze~ ChatGPT-N~ 3          mehrmals p~ ""         
       9 cg         <NA>       cg_nutze~ ChatGPT-N~ 4          mehrmals p~ ""         
      10 cg         <NA>       cg_nutze~ ChatGPT-N~ 5          fast tägli~ ""         
      # i 81 more rows
      # i 2 more variables: item_infix <chr>, item_postfix <chr>

# Missing labels

    Code
      .
    Output
      # A tibble: 0 x 9
      # i 9 variables: item_group <chr>, item_class <chr>, item_name <chr>,
      #   item_label <chr>, value_label <lgl>, value_name <lgl>, item_prefix <lgl>,
      #   item_infix <lgl>, item_postfix <chr>

