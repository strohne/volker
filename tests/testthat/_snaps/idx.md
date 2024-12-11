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
      

# factors are added

    Code
      data %>% add_factors(tidyselect::starts_with("cg_adoption"), k = NULL) %>%
        factor_tab(starts_with("fct_cg_adoption"))
    Output
      
      
      |Expectations                                                | Component 1| Component 2| communality|
      |:-----------------------------------------------------------|-----------:|-----------:|-----------:|
      |ChatGPT has clear advantages compared to similar offerings. |         0.7|        -0.1|         0.5|
      |Using ChatGPT brings financial benefits.                    |         0.7|         0.3|         0.6|
      |Using ChatGPT is advantageous in many tasks.                |         0.7|        -0.1|         0.5|
      |Compared to other systems, using ChatGPT is more fun.       |         0.7|        -0.1|         0.5|
      |Much can go wrong when using ChatGPT.                       |        -0.3|         0.7|         0.6|
      |There are legal issues with using ChatGPT.                  |         0.3|         0.6|         0.4|
      |The security of user data is not guaranteed with ChatGPT.   |         0.1|         0.7|         0.5|
      |Using ChatGPT could bring personal disadvantages.           |         0.0|         0.7|         0.6|
      |In my environment, using ChatGPT is standard.               |         0.7|         0.2|         0.6|
      |Almost everyone in my environment uses ChatGPT.             |         0.7|         0.4|         0.6|
      |Not using ChatGPT is considered being an outsider.          |         0.5|         0.5|         0.5|
      |Using ChatGPT brings me recognition from my environment.    |         0.7|         0.2|         0.5|
      
      4 missing case(s) omitted.
      
      
      
      |Component   | Eigenvalue| Proportion of variance| Cumulative proportion of variance|
      |:-----------|----------:|----------------------:|---------------------------------:|
      |Component 1 |        3.8|                    0.3|                               0.3|
      |Component 2 |        2.5|                    0.2|                               0.5|
      
      
      |Test          |                Statistic|  value|
      |:-------------|------------------------:|------:|
      |KMO Test      |                    Cases|     97|
      |KMO Test      |                Variables|     12|
      |KMO Test      | Cases-to-Variables Ratio|   8.08|
      |KMO Test      |              Overall MSA|   0.74|
      |Bartlett Test |              Chi-squared| 463.54|
      |Bartlett Test |                       df|     66|
      |Bartlett Test |                        p|  0.000|
      |Bartlett Test |                    stars|    ***|
      
      Eigenvalues for scree plot
      
      |Component | Eigenvalue|
      |:---------|----------:|
      |1         |        4.2|
      |2         |        2.1|
      |3         |        1.4|
      |4         |        0.8|
      |5         |        0.7|
      |6         |        0.6|
      |7         |        0.5|
      |8         |        0.5|
      |9         |        0.4|
      |10        |        0.4|
      |11        |        0.3|
      |12        |        0.2|
      
      Automatically selected k=2 by comparing eigenvalues with random data.
      

# clusters are added

    Code
      data %>% add_clusters(tidyselect::starts_with("cg_adoption"), k = NULL) %>%
        cluster_tab(cls_cg_adoption)
    Output
      
      
      |Expectations                                                |     total| Cluster 1| Cluster 2|
      |:-----------------------------------------------------------|---------:|---------:|---------:|
      |ChatGPT has clear advantages compared to similar offerings. | 3.4 (1.0)| 3.2 (1.0)| 3.8 (0.9)|
      |Using ChatGPT brings financial benefits.                    | 2.7 (1.2)| 2.2 (1.1)| 3.5 (0.9)|
      |Using ChatGPT is advantageous in many tasks.                | 3.6 (1.1)| 3.3 (1.2)| 4.0 (0.8)|
      |Compared to other systems, using ChatGPT is more fun.       | 3.5 (1.0)| 3.2 (1.0)| 4.0 (0.8)|
      |Much can go wrong when using ChatGPT.                       | 3.1 (1.1)| 3.1 (1.1)| 3.1 (1.1)|
      |There are legal issues with using ChatGPT.                  | 3.1 (1.2)| 2.8 (1.2)| 3.5 (1.0)|
      |The security of user data is not guaranteed with ChatGPT.   | 3.2 (1.0)| 3.0 (1.0)| 3.5 (1.0)|
      |Using ChatGPT could bring personal disadvantages.           | 2.7 (1.1)| 2.5 (1.0)| 3.1 (1.2)|
      |In my environment, using ChatGPT is standard.               | 2.5 (1.1)| 1.9 (0.8)| 3.5 (0.9)|
      |Almost everyone in my environment uses ChatGPT.             | 2.4 (1.2)| 1.8 (0.8)| 3.4 (1.0)|
      |Not using ChatGPT is considered being an outsider.          | 2.0 (1.2)| 1.4 (0.6)| 2.9 (1.2)|
      |Using ChatGPT brings me recognition from my environment.    | 2.3 (1.2)| 1.7 (0.8)| 3.4 (1.1)|
      
      4 missing case(s) omitted.
      
      
      
      |Cluster   |  n|    p|
      |:---------|--:|----:|
      |Cluster 1 | 61|  63%|
      |Cluster 2 | 36|  37%|
      |total     | 97| 100%|
      
      
      |Statistic                      |  Value|
      |:------------------------------|------:|
      |Within-Cluster Sum of Squares  | 907.02|
      |Between-Cluster Sum of Squares | 244.98|
      
      Within-Cluster Sum of Squares for Scree Plot
      
      |Clusters k |    WSS|
      |:----------|------:|
      |1          | 1152.0|
      |2          |  907.0|
      |3          |  799.1|
      |4          |  729.5|
      |5          |  694.3|
      |6          |  643.0|
      |7          |  602.8|
      |8          |  569.5|
      |9          |  543.4|
      |10         |  501.8|
      
      Automatically selected k=2 by the elbow criterion.
      

