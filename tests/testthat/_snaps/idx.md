# idx_add is deprecated

    Code
      data %>% idx_add(tidyselect::starts_with("cg_adoption")) %>%
        tab_metrics_one_grouped(idx_cg_adoption, adopter)
    Condition
      Warning:
      `idx_add()` was deprecated in volker 3.0.0.
      i Please use `add_index()` instead.
    Output
      
      
      |Innovator type                                    | min|  q1| median|  q3| max| mean|  sd|  n| items| alpha|
      |:-------------------------------------------------|---:|---:|------:|---:|---:|----:|---:|--:|-----:|-----:|
      |I try new offers immediately                      | 1.5| 3.2|    3.3| 4.1| 5.0|  3.5| 0.9| 15|    12|  0.81|
      |I try new offers rather quickly                   | 1.8| 2.5|    2.8| 3.1| 3.8|  2.8| 0.5| 61|    12|  0.81|
      |I wait until offers establish themselves          | 1.0| 2.4|    2.7| 3.0| 3.8|  2.7| 0.6| 20|    12|  0.81|
      |I only use new offers when I have no other choice | 2.4| 2.4|    2.4| 2.4| 2.4|  2.4|    |  1|    12|  0.81|
      |total                                             | 1.0| 2.4|    2.8| 3.2| 5.0|  2.9| 0.6| 97|    12|  0.81|
      
      4 missing case(s) omitted.
      

---

    Code
      data %>% add_index(tidyselect::starts_with("cg_adoption")) %>%
        tab_metrics_one_grouped(idx_cg_adoption, adopter)
    Output
      
      
      |Innovator type                                    | min|  q1| median|  q3| max| mean|  sd|  n| items| alpha|
      |:-------------------------------------------------|---:|---:|------:|---:|---:|----:|---:|--:|-----:|-----:|
      |I try new offers immediately                      | 1.5| 3.2|    3.3| 4.1| 5.0|  3.5| 0.9| 15|    12|  0.81|
      |I try new offers rather quickly                   | 1.8| 2.5|    2.8| 3.1| 3.8|  2.8| 0.5| 61|    12|  0.81|
      |I wait until offers establish themselves          | 1.0| 2.4|    2.7| 3.0| 3.8|  2.7| 0.6| 20|    12|  0.81|
      |I only use new offers when I have no other choice | 2.4| 2.4|    2.4| 2.4| 2.4|  2.4|    |  1|    12|  0.81|
      |total                                             | 1.0| 2.4|    2.8| 3.2| 5.0|  2.9| 0.6| 97|    12|  0.81|
      
      4 missing case(s) omitted.
      

