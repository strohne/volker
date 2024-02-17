# Labels

    Code
      volker::codebook(data)
    Output
      # A tibble: 94 x 6
         item_name     item_group item_class item_label         value_name value_label
         <chr>         <chr>      <chr>      <chr>              <chr>      <chr>      
       1 case          case       <NA>       case               <NA>       <NA>       
       2 sd_age        sd         <NA>       Age                <NA>       <NA>       
       3 cg_activities cg         <NA>       Activities with C~ <NA>       <NA>       
       4 use_private   use        <NA>       Usage: in private~ 1          never      
       5 use_private   use        <NA>       Usage: in private~ 2          rarely     
       6 use_private   use        <NA>       Usage: in private~ 3          several ti~
       7 use_private   use        <NA>       Usage: in private~ 4          several ti~
       8 use_private   use        <NA>       Usage: in private~ 5          almost dai~
       9 use_work      use        <NA>       Usage: in profess~ 1          never      
      10 use_work      use        <NA>       Usage: in profess~ 2          rarely     
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
      # A tibble: 26 x 6
         item_name             item_group item_class item_label value_name value_label
         <chr>                 <chr>      <chr>      <chr>      <chr>      <chr>      
       1 case                  case       <NA>       case       <NA>       <NA>       
       2 use_private           use        <NA>       use_priva~ <NA>       <NA>       
       3 use_work              use        <NA>       use_work   <NA>       <NA>       
       4 cg_adoption_advantag~ cg         <NA>       cg_adopti~ <NA>       <NA>       
       5 cg_adoption_advantag~ cg         <NA>       cg_adopti~ <NA>       <NA>       
       6 cg_adoption_advantag~ cg         <NA>       cg_adopti~ <NA>       <NA>       
       7 cg_adoption_advantag~ cg         <NA>       cg_adopti~ <NA>       <NA>       
       8 cg_adoption_fearofus~ cg         <NA>       cg_adopti~ <NA>       <NA>       
       9 cg_adoption_fearofus~ cg         <NA>       cg_adopti~ <NA>       <NA>       
      10 cg_adoption_fearofus~ cg         <NA>       cg_adopti~ <NA>       <NA>       
      # i 16 more rows

# Store, clear and restore the codebook

    Code
      .
    Output
      # A tibble: 94 x 6
         item_name     item_group item_class item_label         value_name value_label
         <chr>         <chr>      <chr>      <chr>              <chr>      <chr>      
       1 case          case       <NA>       case               <NA>       <NA>       
       2 sd_age        sd         <NA>       Age                <NA>       <NA>       
       3 cg_activities cg         <NA>       Activities with C~ <NA>       <NA>       
       4 use_private   use        <NA>       Usage: in private~ 1          never      
       5 use_private   use        <NA>       Usage: in private~ 2          rarely     
       6 use_private   use        <NA>       Usage: in private~ 3          several ti~
       7 use_private   use        <NA>       Usage: in private~ 4          several ti~
       8 use_private   use        <NA>       Usage: in private~ 5          almost dai~
       9 use_work      use        <NA>       Usage: in profess~ 1          never      
      10 use_work      use        <NA>       Usage: in profess~ 2          rarely     
      # i 84 more rows

# Item labels are replaced and keep their order

    Code
      levels(dplyr::pull(volker:::labs_replace_values(dplyr::select(data, adopter),
      adopter, volker::codebook(data)), adopter))
    Output
      [1] "I try new offers immediately"                     
      [2] "I try new offers rather quickly"                  
      [3] "I wait until offers establish themselves"         
      [4] "I only use new offers when I have no other choice"
      [5] "[no answer]"                                      

