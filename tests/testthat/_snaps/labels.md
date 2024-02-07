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

# Store and clear the codebook

    Code
      .
    Output
      # A tibble: 19 x 6
         item_name             item_group item_class item_label value_label value_name
         <chr>                 <chr>      <chr>      <chr>      <lgl>       <lgl>     
       1 case                  case       <NA>       case       NA          NA        
       2 cg_nutzen_privat      cg         <NA>       cg_nutzen~ NA          NA        
       3 cg_nutzen_beruflich   cg         <NA>       cg_nutzen~ NA          NA        
       4 cg_adoption_advantag~ cg         <NA>       cg_adopti~ NA          NA        
       5 cg_adoption_advantag~ cg         <NA>       cg_adopti~ NA          NA        
       6 cg_adoption_advantag~ cg         <NA>       cg_adopti~ NA          NA        
       7 cg_adoption_advantag~ cg         <NA>       cg_adopti~ NA          NA        
       8 cg_adoption_fearofus~ cg         <NA>       cg_adopti~ NA          NA        
       9 cg_adoption_fearofus~ cg         <NA>       cg_adopti~ NA          NA        
      10 cg_adoption_fearofus~ cg         <NA>       cg_adopti~ NA          NA        
      11 cg_adoption_fearofus~ cg         <NA>       cg_adopti~ NA          NA        
      12 cg_adoption_social_01 cg         <NA>       cg_adopti~ NA          NA        
      13 cg_adoption_social_02 cg         <NA>       cg_adopti~ NA          NA        
      14 cg_adoption_social_03 cg         <NA>       cg_adopti~ NA          NA        
      15 cg_adoption_social_04 cg         <NA>       cg_adopti~ NA          NA        
      16 in_adoption           in         <NA>       in_adopti~ NA          NA        
      17 sd_alter              sd         <NA>       sd_alter   NA          NA        
      18 sd_geschlecht         sd         <NA>       sd_geschl~ NA          NA        
      19 cg_activities         cg         <NA>       cg_activi~ NA          NA        

# Store, clear and restore the codebook

    Code
      .
    Output
      # A tibble: 87 x 6
         item_name        item_group item_class item_label      value_name value_label
         <chr>            <chr>      <chr>      <chr>           <chr>      <chr>      
       1 case             case       <NA>       case            <NA>       <NA>       
       2 in_adoption      in         <NA>       Innovationstyp  <NA>       <NA>       
       3 sd_alter         sd         <NA>       Alter           <NA>       <NA>       
       4 sd_geschlecht    sd         <NA>       Geschlecht      <NA>       <NA>       
       5 cg_activities    cg         <NA>       ChatGPT-Aktivi~ <NA>       <NA>       
       6 cg_nutzen_privat cg         <NA>       ChatGPT-Nutzun~ 1          nie        
       7 cg_nutzen_privat cg         <NA>       ChatGPT-Nutzun~ 2          seltener   
       8 cg_nutzen_privat cg         <NA>       ChatGPT-Nutzun~ 3          mehrmals p~
       9 cg_nutzen_privat cg         <NA>       ChatGPT-Nutzun~ 4          mehrmals p~
      10 cg_nutzen_privat cg         <NA>       ChatGPT-Nutzun~ 5          fast tägli~
      # i 77 more rows

