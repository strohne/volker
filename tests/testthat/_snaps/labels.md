# Labels are retrieved

    Code
      volker::codebook(data)
    Output
      # A tibble: 94 x 6
         item_name     item_group item_class item_label         value_name value_label
         <chr>         <chr>      <chr>      <chr>              <chr>      <chr>      
       1 case          case       numeric    case               <NA>       <NA>       
       2 sd_age        sd         numeric    Age                <NA>       <NA>       
       3 cg_activities cg         character  Activities with C~ <NA>       <NA>       
       4 use_private   use        numeric    Usage: in private~ 1          never      
       5 use_private   use        numeric    Usage: in private~ 2          rarely     
       6 use_private   use        numeric    Usage: in private~ 3          several ti~
       7 use_private   use        numeric    Usage: in private~ 4          several ti~
       8 use_private   use        numeric    Usage: in private~ 5          almost dai~
       9 use_work      use        numeric    Usage: in profess~ 1          never      
      10 use_work      use        numeric    Usage: in profess~ 2          rarely     
      # i 84 more rows

# Missing labels make no trouble

    Code
      .
    Output
      # A tibble: 12 x 6
         item_name             item_group item_class item_label value_label value_name
         <chr>                 <chr>      <chr>      <chr>      <lgl>       <lgl>     
       1 cg_adoption_advantag~ cg         numeric    cg_adopti~ NA          NA        
       2 cg_adoption_advantag~ cg         numeric    cg_adopti~ NA          NA        
       3 cg_adoption_advantag~ cg         numeric    cg_adopti~ NA          NA        
       4 cg_adoption_advantag~ cg         numeric    cg_adopti~ NA          NA        
       5 cg_adoption_fearofus~ cg         numeric    cg_adopti~ NA          NA        
       6 cg_adoption_fearofus~ cg         numeric    cg_adopti~ NA          NA        
       7 cg_adoption_fearofus~ cg         numeric    cg_adopti~ NA          NA        
       8 cg_adoption_fearofus~ cg         numeric    cg_adopti~ NA          NA        
       9 cg_adoption_social_01 cg         numeric    cg_adopti~ NA          NA        
      10 cg_adoption_social_02 cg         numeric    cg_adopti~ NA          NA        
      11 cg_adoption_social_03 cg         numeric    cg_adopti~ NA          NA        
      12 cg_adoption_social_04 cg         numeric    cg_adopti~ NA          NA        

# Store and clear the codebook

    Code
      data %>% volker::labs_store() %>% volker::labs_clear() %>% codebook() %>% print(
        n = Inf)
    Output
      # A tibble: 26 x 6
         item_name             item_group item_class item_label value_name value_label
         <chr>                 <chr>      <chr>      <chr>      <chr>      <chr>      
       1 case                  case       numeric    case       <NA>       <NA>       
       2 use_private           use        numeric    use_priva~ <NA>       <NA>       
       3 use_work              use        numeric    use_work   <NA>       <NA>       
       4 cg_adoption_advantag~ cg         numeric    cg_adopti~ <NA>       <NA>       
       5 cg_adoption_advantag~ cg         numeric    cg_adopti~ <NA>       <NA>       
       6 cg_adoption_advantag~ cg         numeric    cg_adopti~ <NA>       <NA>       
       7 cg_adoption_advantag~ cg         numeric    cg_adopti~ <NA>       <NA>       
       8 cg_adoption_fearofus~ cg         numeric    cg_adopti~ <NA>       <NA>       
       9 cg_adoption_fearofus~ cg         numeric    cg_adopti~ <NA>       <NA>       
      10 cg_adoption_fearofus~ cg         numeric    cg_adopti~ <NA>       <NA>       
      11 cg_adoption_fearofus~ cg         numeric    cg_adopti~ <NA>       <NA>       
      12 cg_adoption_social_01 cg         numeric    cg_adopti~ <NA>       <NA>       
      13 cg_adoption_social_02 cg         numeric    cg_adopti~ <NA>       <NA>       
      14 cg_adoption_social_03 cg         numeric    cg_adopti~ <NA>       <NA>       
      15 cg_adoption_social_04 cg         numeric    cg_adopti~ <NA>       <NA>       
      16 sd_age                sd         numeric    sd_age     <NA>       <NA>       
      17 cg_activities         cg         character  cg_activi~ <NA>       <NA>       
      18 adopter               adopter    factor     adopter    I try new~ I try new ~
      19 adopter               adopter    factor     adopter    I try new~ I try new ~
      20 adopter               adopter    factor     adopter    I wait un~ I wait unt~
      21 adopter               adopter    factor     adopter    I only us~ I only use~
      22 adopter               adopter    factor     adopter    [no answe~ [no answer]
      23 sd_gender             sd         factor     sd_gender  female     female     
      24 sd_gender             sd         factor     sd_gender  male       male       
      25 sd_gender             sd         factor     sd_gender  diverse    diverse    
      26 sd_gender             sd         factor     sd_gender  [no answe~ [no answer]

# Store, clear and restore the codebook

    Code
      data %>% volker::labs_store() %>% volker::labs_clear() %>% volker::labs_restore() %>%
        codebook() %>% print(n = Inf)
    Output
      # A tibble: 94 x 6
         item_name             item_group item_class item_label value_name value_label
         <chr>                 <chr>      <chr>      <chr>      <chr>      <chr>      
       1 case                  case       numeric    case       <NA>       <NA>       
       2 sd_age                sd         numeric    Age        <NA>       <NA>       
       3 cg_activities         cg         character  Activitie~ <NA>       <NA>       
       4 use_private           use        numeric    Usage: in~ 1          never      
       5 use_private           use        numeric    Usage: in~ 2          rarely     
       6 use_private           use        numeric    Usage: in~ 3          several ti~
       7 use_private           use        numeric    Usage: in~ 4          several ti~
       8 use_private           use        numeric    Usage: in~ 5          almost dai~
       9 use_work              use        numeric    Usage: in~ 1          never      
      10 use_work              use        numeric    Usage: in~ 2          rarely     
      11 use_work              use        numeric    Usage: in~ 3          several ti~
      12 use_work              use        numeric    Usage: in~ 4          several ti~
      13 use_work              use        numeric    Usage: in~ 5          almost dai~
      14 cg_adoption_advantag~ cg         numeric    Expectati~ 1          strongly d~
      15 cg_adoption_advantag~ cg         numeric    Expectati~ 2          disagree   
      16 cg_adoption_advantag~ cg         numeric    Expectati~ 3          neutral    
      17 cg_adoption_advantag~ cg         numeric    Expectati~ 4          agree      
      18 cg_adoption_advantag~ cg         numeric    Expectati~ 5          strongly a~
      19 cg_adoption_advantag~ cg         numeric    Expectati~ -1         [no answer]
      20 cg_adoption_advantag~ cg         numeric    Expectati~ 1          strongly d~
      21 cg_adoption_advantag~ cg         numeric    Expectati~ 2          disagree   
      22 cg_adoption_advantag~ cg         numeric    Expectati~ 3          neutral    
      23 cg_adoption_advantag~ cg         numeric    Expectati~ 4          agree      
      24 cg_adoption_advantag~ cg         numeric    Expectati~ 5          strongly a~
      25 cg_adoption_advantag~ cg         numeric    Expectati~ -1         [no answer]
      26 cg_adoption_advantag~ cg         numeric    Expectati~ 1          strongly d~
      27 cg_adoption_advantag~ cg         numeric    Expectati~ 2          disagree   
      28 cg_adoption_advantag~ cg         numeric    Expectati~ 3          neutral    
      29 cg_adoption_advantag~ cg         numeric    Expectati~ 4          agree      
      30 cg_adoption_advantag~ cg         numeric    Expectati~ 5          strongly a~
      31 cg_adoption_advantag~ cg         numeric    Expectati~ -1         [no answer]
      32 cg_adoption_advantag~ cg         numeric    Expectati~ 1          strongly d~
      33 cg_adoption_advantag~ cg         numeric    Expectati~ 2          disagree   
      34 cg_adoption_advantag~ cg         numeric    Expectati~ 3          neutral    
      35 cg_adoption_advantag~ cg         numeric    Expectati~ 4          agree      
      36 cg_adoption_advantag~ cg         numeric    Expectati~ 5          strongly a~
      37 cg_adoption_advantag~ cg         numeric    Expectati~ -1         [no answer]
      38 cg_adoption_fearofus~ cg         numeric    Expectati~ 1          strongly d~
      39 cg_adoption_fearofus~ cg         numeric    Expectati~ 2          disagree   
      40 cg_adoption_fearofus~ cg         numeric    Expectati~ 3          neutral    
      41 cg_adoption_fearofus~ cg         numeric    Expectati~ 4          agree      
      42 cg_adoption_fearofus~ cg         numeric    Expectati~ 5          strongly a~
      43 cg_adoption_fearofus~ cg         numeric    Expectati~ -1         [no answer]
      44 cg_adoption_fearofus~ cg         numeric    Expectati~ 1          strongly d~
      45 cg_adoption_fearofus~ cg         numeric    Expectati~ 2          disagree   
      46 cg_adoption_fearofus~ cg         numeric    Expectati~ 3          neutral    
      47 cg_adoption_fearofus~ cg         numeric    Expectati~ 4          agree      
      48 cg_adoption_fearofus~ cg         numeric    Expectati~ 5          strongly a~
      49 cg_adoption_fearofus~ cg         numeric    Expectati~ -1         [no answer]
      50 cg_adoption_fearofus~ cg         numeric    Expectati~ 1          strongly d~
      51 cg_adoption_fearofus~ cg         numeric    Expectati~ 2          disagree   
      52 cg_adoption_fearofus~ cg         numeric    Expectati~ 3          neutral    
      53 cg_adoption_fearofus~ cg         numeric    Expectati~ 4          agree      
      54 cg_adoption_fearofus~ cg         numeric    Expectati~ 5          strongly a~
      55 cg_adoption_fearofus~ cg         numeric    Expectati~ -1         [no answer]
      56 cg_adoption_fearofus~ cg         numeric    Expectati~ 1          strongly d~
      57 cg_adoption_fearofus~ cg         numeric    Expectati~ 2          disagree   
      58 cg_adoption_fearofus~ cg         numeric    Expectati~ 3          neutral    
      59 cg_adoption_fearofus~ cg         numeric    Expectati~ 4          agree      
      60 cg_adoption_fearofus~ cg         numeric    Expectati~ 5          strongly a~
      61 cg_adoption_fearofus~ cg         numeric    Expectati~ -1         [no answer]
      62 cg_adoption_social_01 cg         numeric    Expectati~ 1          strongly d~
      63 cg_adoption_social_01 cg         numeric    Expectati~ 2          disagree   
      64 cg_adoption_social_01 cg         numeric    Expectati~ 3          neutral    
      65 cg_adoption_social_01 cg         numeric    Expectati~ 4          agree      
      66 cg_adoption_social_01 cg         numeric    Expectati~ 5          strongly a~
      67 cg_adoption_social_01 cg         numeric    Expectati~ -1         [no answer]
      68 cg_adoption_social_02 cg         numeric    Expectati~ 1          strongly d~
      69 cg_adoption_social_02 cg         numeric    Expectati~ 2          disagree   
      70 cg_adoption_social_02 cg         numeric    Expectati~ 3          neutral    
      71 cg_adoption_social_02 cg         numeric    Expectati~ 4          agree      
      72 cg_adoption_social_02 cg         numeric    Expectati~ 5          strongly a~
      73 cg_adoption_social_02 cg         numeric    Expectati~ -1         [no answer]
      74 cg_adoption_social_03 cg         numeric    Expectati~ 1          strongly d~
      75 cg_adoption_social_03 cg         numeric    Expectati~ 2          disagree   
      76 cg_adoption_social_03 cg         numeric    Expectati~ 3          neutral    
      77 cg_adoption_social_03 cg         numeric    Expectati~ 4          agree      
      78 cg_adoption_social_03 cg         numeric    Expectati~ 5          strongly a~
      79 cg_adoption_social_03 cg         numeric    Expectati~ -1         [no answer]
      80 cg_adoption_social_04 cg         numeric    Expectati~ 1          strongly d~
      81 cg_adoption_social_04 cg         numeric    Expectati~ 2          disagree   
      82 cg_adoption_social_04 cg         numeric    Expectati~ 3          neutral    
      83 cg_adoption_social_04 cg         numeric    Expectati~ 4          agree      
      84 cg_adoption_social_04 cg         numeric    Expectati~ 5          strongly a~
      85 cg_adoption_social_04 cg         numeric    Expectati~ -1         [no answer]
      86 adopter               adopter    factor     Innovator~ I try new~ I try new ~
      87 adopter               adopter    factor     Innovator~ I try new~ I try new ~
      88 adopter               adopter    factor     Innovator~ I wait un~ I wait unt~
      89 adopter               adopter    factor     Innovator~ I only us~ I only use~
      90 adopter               adopter    factor     Innovator~ [no answe~ [no answer]
      91 sd_gender             sd         factor     Gender     female     female     
      92 sd_gender             sd         factor     Gender     male       male       
      93 sd_gender             sd         factor     Gender     diverse    diverse    
      94 sd_gender             sd         factor     Gender     [no answe~ [no answer]

# Item values are replaced and keep their order

    Code
      levels(dplyr::pull(volker:::labs_replace(dplyr::select(data, adopter), adopter,
      volker::codebook(data, adopter)), adopter))
    Output
      [1] "I try new offers immediately"                     
      [2] "I try new offers rather quickly"                  
      [3] "I wait until offers establish themselves"         
      [4] "I only use new offers when I have no other choice"

# Item values are kept even if they are not in the codebook

    Code
      dplyr::arrange(volker:::labs_replace(dplyr::mutate(dplyr::distinct(data, from = use_private),
      to = from), to, codes), to)
    Output
      # A tibble: 5 x 2
         from to          
        <dbl> <fct>       
      1     1 never       
      2     2 2           
      3     3 3           
      4     4 4           
      5     5 almost daily

# A common prefix is removed from labels

    Code
      get_prefix(dplyr::pull(codebook(dplyr::select(data, starts_with("use"))),
      item_label))
    Output
      [1] "Usage"

---

    Code
      trim_prefix(dplyr::pull(codebook(dplyr::select(data, starts_with("use"))),
      item_label))
    Output
       [1] "in private context"      "in private context"     
       [3] "in private context"      "in private context"     
       [5] "in private context"      "in professional context"
       [7] "in professional context" "in professional context"
       [9] "in professional context" "in professional context"

