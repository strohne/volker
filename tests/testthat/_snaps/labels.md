# Labels

    Code
      volker::get_codebook(data)
    Output
      # A tibble: 94 x 9
         item_group item_class item_name item_label value_name value_label item_prefix
         <chr>      <chr>      <chr>     <chr>      <chr>      <chr>       <chr>      
       1 case       <NA>       case      case       <NA>       <NA>        ""         
       2 sd         <NA>       sd_alter  sd_alter   <NA>       <NA>        ""         
       3 cg         <NA>       cg_activ~ cg_activi~ <NA>       <NA>        ""         
       4 cg         <NA>       cg_nutze~ ChatGPT-N~ 1          nie         ""         
       5 cg         <NA>       cg_nutze~ ChatGPT-N~ 2          seltener    ""         
       6 cg         <NA>       cg_nutze~ ChatGPT-N~ 3          mehrmals p~ ""         
       7 cg         <NA>       cg_nutze~ ChatGPT-N~ 4          mehrmals p~ ""         
       8 cg         <NA>       cg_nutze~ ChatGPT-N~ 5          fast tägli~ ""         
       9 cg         <NA>       cg_nutze~ ChatGPT-N~ 1          nie         ""         
      10 cg         <NA>       cg_nutze~ ChatGPT-N~ 2          seltener    ""         
      # i 84 more rows
      # i 2 more variables: item_infix <chr>, item_postfix <chr>

# Missing labels

    Code
      .
    Output
      # A tibble: 12 x 9
         item_group item_class item_name item_label value_label value_name item_prefix
         <chr>      <chr>      <chr>     <chr>      <lgl>       <lgl>      <chr>      
       1 cg         <NA>       cg_adopt~ cg_adopti~ NA          NA         cg_adoptio~
       2 cg         <NA>       cg_adopt~ cg_adopti~ NA          NA         cg_adoptio~
       3 cg         <NA>       cg_adopt~ cg_adopti~ NA          NA         cg_adoptio~
       4 cg         <NA>       cg_adopt~ cg_adopti~ NA          NA         cg_adoptio~
       5 cg         <NA>       cg_adopt~ cg_adopti~ NA          NA         cg_adoptio~
       6 cg         <NA>       cg_adopt~ cg_adopti~ NA          NA         cg_adoptio~
       7 cg         <NA>       cg_adopt~ cg_adopti~ NA          NA         cg_adoptio~
       8 cg         <NA>       cg_adopt~ cg_adopti~ NA          NA         cg_adoptio~
       9 cg         <NA>       cg_adopt~ cg_adopti~ NA          NA         cg_adoptio~
      10 cg         <NA>       cg_adopt~ cg_adopti~ NA          NA         cg_adoptio~
      11 cg         <NA>       cg_adopt~ cg_adopti~ NA          NA         cg_adoptio~
      12 cg         <NA>       cg_adopt~ cg_adopti~ NA          NA         cg_adoptio~
      # i 2 more variables: item_infix <chr>, item_postfix <chr>

