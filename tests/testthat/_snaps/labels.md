# Labels

    Code
      volker::codebook(data)
    Output
      # A tibble: 94 x 6
         item_name           item_group item_class item_label   value_name value_label
         <chr>               <chr>      <chr>      <chr>        <chr>      <chr>      
       1 case                case       <NA>       case         <NA>       <NA>       
       2 sd_alter            sd         <NA>       Alter        <NA>       <NA>       
       3 cg_activities       cg         <NA>       ChatGPT-Akt~ <NA>       <NA>       
       4 cg_nutzen_privat    cg         <NA>       ChatGPT-Nut~ 1          nie        
       5 cg_nutzen_privat    cg         <NA>       ChatGPT-Nut~ 2          seltener   
       6 cg_nutzen_privat    cg         <NA>       ChatGPT-Nut~ 3          mehrmals p~
       7 cg_nutzen_privat    cg         <NA>       ChatGPT-Nut~ 4          mehrmals p~
       8 cg_nutzen_privat    cg         <NA>       ChatGPT-Nut~ 5          fast tägli~
       9 cg_nutzen_beruflich cg         <NA>       ChatGPT-Nut~ 1          nie        
      10 cg_nutzen_beruflich cg         <NA>       ChatGPT-Nut~ 2          seltener   
      # i 84 more rows

# Missing labels

    Code
      .
    Output
      # A tibble: 12 x 6
         item_name             item_group item_class item_label value_label value_name
         <chr>                 <chr>      <chr>      <chr>      <lgl>       <lgl>     
       1 cg_adoption_advantag~ cg         <NA>       cg_adopti~ NA          NA        
       2 cg_adoption_advantag~ cg         <NA>       cg_adopti~ NA          NA        
       3 cg_adoption_advantag~ cg         <NA>       cg_adopti~ NA          NA        
       4 cg_adoption_advantag~ cg         <NA>       cg_adopti~ NA          NA        
       5 cg_adoption_fearofus~ cg         <NA>       cg_adopti~ NA          NA        
       6 cg_adoption_fearofus~ cg         <NA>       cg_adopti~ NA          NA        
       7 cg_adoption_fearofus~ cg         <NA>       cg_adopti~ NA          NA        
       8 cg_adoption_fearofus~ cg         <NA>       cg_adopti~ NA          NA        
       9 cg_adoption_social_01 cg         <NA>       cg_adopti~ NA          NA        
      10 cg_adoption_social_02 cg         <NA>       cg_adopti~ NA          NA        
      11 cg_adoption_social_03 cg         <NA>       cg_adopti~ NA          NA        
      12 cg_adoption_social_04 cg         <NA>       cg_adopti~ NA          NA        

