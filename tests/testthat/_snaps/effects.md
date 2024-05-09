# effect_counts_one_grouped

    Code
      volker::effect_counts(mutate(filter(data, sd_gender != "diverse"), sd_age = ifelse(
        sd_age > 40, "+40", "-40")), sd_gender, sd_age)
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"n"` instead of `.data$n`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"n"` instead of `.data$n`
    Output
      
      
      |Statistic          | Value|
      |:------------------|-----:|
      |Number of cases    |   100|
      |Cramer's V         |  0.00|
      |Degrees of freedom |     1|
      |Chi-squared        |  0.20|
      |p value            | 0.651|
      |stars              |      |

# effect_metrics_one_grouped

    Code
      volker::effect_metrics(data, sd_age, adopter)
    Output
      
      
      |Term                                              | estimate| conf.low| conf.high| std.error|     t|     p| stars|
      |:-------------------------------------------------|--------:|--------:|---------:|---------:|-----:|-----:|-----:|
      |(Intercept)                                       |    36.60|    29.55|     43.65|      3.55| 10.30| 0.000|   ***|
      |I try new offers immediately (Reference)          |         |         |          |          |      |      |      |
      |I try new offers rather quickly                   |     4.21|    -3.64|     12.06|      3.95|  1.06| 0.290|      |
      |I wait until offers establish themselves          |     1.17|    -7.97|     10.32|      4.61|  0.25| 0.800|      |
      |I only use new offers when I have no other choice |    19.40|    -8.81|     47.61|     14.21|  1.36| 0.175|      |
      
      
      |Statistic                     | value|
      |:-----------------------------|-----:|
      |Adjusted R squared            |     0|
      |Degrees of freedom            |     3|
      |Residuals' degrees of freedom |    97|
      |F                             |     1|
      |p                             |   0.4|
      |stars                         |      |

# effect_metrics_one_cor

    Code
      volker::effect_metrics(data, sd_age, use_private, metric = TRUE)
    Output
      
      
      |Statistic   | value|
      |:-----------|-----:|
      |n           |   101|
      |Pearson's r | -0.19|
      |ci.low      | -0.37|
      |ci.high     |  0.01|
      |df          |    99|
      |t           | -1.88|
      |p           | 0.063|
      |stars       |     .|

# effect_metrics_one_cor with spearman

    Code
      volker::effect_metrics(data, sd_age, use_private, metric = TRUE, method = "spearman")
    Output
      
      
      |Statistic      |     value|
      |:--------------|---------:|
      |n              |       101|
      |Spearman's rho |      -0.2|
      |s              | 206410.07|
      |p              |     0.043|
      |stars          |         *|

---

    Code
      volker::effect_metrics(data, sd_age, use_private, metric = TRUE, method = "spearman")
    Output
      
      
      |Statistic      |     value|
      |:--------------|---------:|
      |n              |       101|
      |Spearman's rho |      -0.2|
      |s              | 206410.07|
      |p              |     0.043|
      |stars          |         *|

# effect_metrics_items

    Code
      volker::effect_metrics(data, tidyselect::starts_with("use_"))
    Output
      
      
      |Item 1                  |                  Item 2|   n| Pearson's r| ci.low| ci.high| df|    t|     p| stars|
      |:-----------------------|-----------------------:|---:|-----------:|------:|-------:|--:|----:|-----:|-----:|
      |in private context      | in professional context| 101|        0.37|   0.19|    0.53| 99| 4.00| 0.000|   ***|
      |in professional context |      in private context| 101|        0.37|   0.19|    0.53| 99| 4.00| 0.000|   ***|

---

    Code
      volker::effect_metrics(data, tidyselect::starts_with("use_"))
    Output
      
      
      |Item 1                  |                  Item 2|   n| Pearson's r| ci.low| ci.high| df|    t|     p| stars|
      |:-----------------------|-----------------------:|---:|-----------:|------:|-------:|--:|----:|-----:|-----:|
      |in private context      | in professional context| 101|        0.37|   0.19|    0.53| 99| 4.00| 0.000|   ***|
      |in professional context |      in private context| 101|        0.37|   0.19|    0.53| 99| 4.00| 0.000|   ***|

# effect_metrics_items_cor with one variable

    Code
      volker::effect_metrics(data, tidyselect::starts_with("cg_adoption_"), sd_age,
      metric = TRUE)
    Output
      
      
      |Item 1                                                                    | Item 2|  n| Pearson's r| ci.low| ci.high| df|     t|     p| stars|
      |:-------------------------------------------------------------------------|------:|--:|-----------:|------:|-------:|--:|-----:|-----:|-----:|
      |Expectations: ChatGPT has clear advantages compared to similar offerings. |    Age| 97|       -0.12|  -0.32|    0.08| 95| -1.22| 0.227|      |
      |Expectations: Using ChatGPT brings financial benefits.                    |    Age| 97|       -0.13|  -0.32|    0.07| 95| -1.29| 0.200|      |
      |Expectations: Using ChatGPT is advantageous in many tasks.                |    Age| 97|       -0.09|  -0.28|    0.11| 95| -0.86| 0.393|      |
      |Expectations: Compared to other systems, using ChatGPT is more fun.       |    Age| 97|       -0.12|  -0.31|    0.08| 95| -1.15| 0.254|      |
      |Expectations: Much can go wrong when using ChatGPT.                       |    Age| 97|        0.18|  -0.02|    0.36| 95|  1.76| 0.082|     .|
      |Expectations: There are legal issues with using ChatGPT.                  |    Age| 97|        0.24|   0.05|    0.42| 95|  2.45| 0.016|     *|
      |Expectations: The security of user data is not guaranteed with ChatGPT.   |    Age| 97|       -0.10|  -0.29|    0.11| 95| -0.93| 0.354|      |
      |Expectations: Using ChatGPT could bring personal disadvantages.           |    Age| 97|        0.02|  -0.18|    0.22| 95|  0.20| 0.846|      |
      |Expectations: In my environment, using ChatGPT is standard.               |    Age| 97|       -0.17|  -0.36|    0.03| 95| -1.73| 0.087|     .|
      |Expectations: Almost everyone in my environment uses ChatGPT.             |    Age| 97|       -0.06|  -0.26|    0.14| 95| -0.59| 0.557|      |
      |Expectations: Not using ChatGPT is considered being an outsider.          |    Age| 97|        0.09|  -0.11|    0.28| 95|  0.87| 0.387|      |
      |Expectations: Using ChatGPT brings me recognition from my environment.    |    Age| 97|       -0.02|  -0.22|    0.18| 95| -0.22| 0.823|      |
      
      5 missing case(s) ommited.
      

# effect_metrics_items with spearman

    Code
      volker::effect_metrics(data, tidyselect::starts_with("use_"), method = "spearman")
    Output
      
      
      |Item 1                  |                  Item 2|   n| Spearman's rho|         s|     p| stars|
      |:-----------------------|-----------------------:|---:|--------------:|---------:|-----:|-----:|
      |in private context      | in professional context| 101|           0.33| 114537.35| 0.001|   ***|
      |in professional context |      in private context| 101|           0.33| 114537.35| 0.001|   ***|

# effect_metrics_items_cor with items

    Code
      volker::effect_metrics(data, tidyselect::starts_with("cg_adoption_"),
      tidyselect::starts_with("cg_adoption_"), metric = TRUE)
    Output
      
      
      |Item 1                                                      |                                                      Item 2|  n| Pearson's r| ci.low| ci.high| df|     t|     p| stars|
      |:-----------------------------------------------------------|-----------------------------------------------------------:|--:|-----------:|------:|-------:|--:|-----:|-----:|-----:|
      |ChatGPT has clear advantages compared to similar offerings. | ChatGPT has clear advantages compared to similar offerings.| 97|        1.00|   1.00|    1.00| 95|   Inf| 0.000|   ***|
      |ChatGPT has clear advantages compared to similar offerings. |                    Using ChatGPT brings financial benefits.| 97|        0.37|   0.19|    0.53| 95|  3.91| 0.000|   ***|
      |ChatGPT has clear advantages compared to similar offerings. |                Using ChatGPT is advantageous in many tasks.| 97|        0.64|   0.50|    0.74| 95|  8.04| 0.000|   ***|
      |ChatGPT has clear advantages compared to similar offerings. |       Compared to other systems, using ChatGPT is more fun.| 97|        0.61|   0.47|    0.72| 95|  7.47| 0.000|   ***|
      |ChatGPT has clear advantages compared to similar offerings. |                       Much can go wrong when using ChatGPT.| 97|       -0.14|  -0.33|    0.06| 95| -1.40| 0.166|      |
      |ChatGPT has clear advantages compared to similar offerings. |                  There are legal issues with using ChatGPT.| 97|        0.19|  -0.01|    0.38| 95|  1.90| 0.061|     .|
      |ChatGPT has clear advantages compared to similar offerings. |   The security of user data is not guaranteed with ChatGPT.| 97|        0.07|  -0.13|    0.27| 95|  0.71| 0.477|      |
      |ChatGPT has clear advantages compared to similar offerings. |           Using ChatGPT could bring personal disadvantages.| 97|        0.01|  -0.19|    0.21| 95|  0.10| 0.924|      |
      |ChatGPT has clear advantages compared to similar offerings. |               In my environment, using ChatGPT is standard.| 97|        0.21|   0.01|    0.39| 95|  2.07| 0.041|     *|
      |ChatGPT has clear advantages compared to similar offerings. |             Almost everyone in my environment uses ChatGPT.| 97|        0.28|   0.08|    0.45| 95|  2.84| 0.006|    **|
      |ChatGPT has clear advantages compared to similar offerings. |          Not using ChatGPT is considered being an outsider.| 97|        0.16|  -0.04|    0.35| 95|  1.59| 0.115|      |
      |ChatGPT has clear advantages compared to similar offerings. |    Using ChatGPT brings me recognition from my environment.| 97|        0.27|   0.07|    0.44| 95|  2.68| 0.009|    **|
      |Using ChatGPT brings financial benefits.                    | ChatGPT has clear advantages compared to similar offerings.| 97|        0.37|   0.19|    0.53| 95|  3.91| 0.000|   ***|
      |Using ChatGPT brings financial benefits.                    |                    Using ChatGPT brings financial benefits.| 97|        1.00|   1.00|    1.00| 95|   Inf| 0.000|   ***|
      |Using ChatGPT brings financial benefits.                    |                Using ChatGPT is advantageous in many tasks.| 97|        0.46|   0.29|    0.61| 95|  5.07| 0.000|   ***|
      |Using ChatGPT brings financial benefits.                    |       Compared to other systems, using ChatGPT is more fun.| 97|        0.42|   0.24|    0.57| 95|  4.55| 0.000|   ***|
      |Using ChatGPT brings financial benefits.                    |                       Much can go wrong when using ChatGPT.| 97|        0.02|  -0.18|    0.22| 95|  0.22| 0.830|      |
      |Using ChatGPT brings financial benefits.                    |                  There are legal issues with using ChatGPT.| 97|        0.34|   0.15|    0.50| 95|  3.52| 0.001|   ***|
      |Using ChatGPT brings financial benefits.                    |   The security of user data is not guaranteed with ChatGPT.| 97|        0.34|   0.15|    0.51| 95|  3.56| 0.001|   ***|
      |Using ChatGPT brings financial benefits.                    |           Using ChatGPT could bring personal disadvantages.| 97|        0.22|   0.02|    0.40| 95|  2.20| 0.030|     *|
      |Using ChatGPT brings financial benefits.                    |               In my environment, using ChatGPT is standard.| 97|        0.54|   0.38|    0.67| 95|  6.24| 0.000|   ***|
      |Using ChatGPT brings financial benefits.                    |             Almost everyone in my environment uses ChatGPT.| 97|        0.50|   0.34|    0.64| 95|  5.67| 0.000|   ***|
      |Using ChatGPT brings financial benefits.                    |          Not using ChatGPT is considered being an outsider.| 97|        0.36|   0.18|    0.53| 95|  3.81| 0.000|   ***|
      |Using ChatGPT brings financial benefits.                    |    Using ChatGPT brings me recognition from my environment.| 97|        0.40|   0.22|    0.55| 95|  4.25| 0.000|   ***|
      |Using ChatGPT is advantageous in many tasks.                | ChatGPT has clear advantages compared to similar offerings.| 97|        0.64|   0.50|    0.74| 95|  8.04| 0.000|   ***|
      |Using ChatGPT is advantageous in many tasks.                |                    Using ChatGPT brings financial benefits.| 97|        0.46|   0.29|    0.61| 95|  5.07| 0.000|   ***|
      |Using ChatGPT is advantageous in many tasks.                |                Using ChatGPT is advantageous in many tasks.| 97|        1.00|   1.00|    1.00| 95|   Inf| 0.000|   ***|
      |Using ChatGPT is advantageous in many tasks.                |       Compared to other systems, using ChatGPT is more fun.| 97|        0.47|   0.30|    0.61| 95|  5.18| 0.000|   ***|
      |Using ChatGPT is advantageous in many tasks.                |                       Much can go wrong when using ChatGPT.| 97|       -0.11|  -0.30|    0.09| 95| -1.10| 0.274|      |
      |Using ChatGPT is advantageous in many tasks.                |                  There are legal issues with using ChatGPT.| 97|        0.19|  -0.01|    0.38| 95|  1.93| 0.056|     .|
      |Using ChatGPT is advantageous in many tasks.                |   The security of user data is not guaranteed with ChatGPT.| 97|        0.06|  -0.14|    0.25| 95|  0.57| 0.573|      |
      |Using ChatGPT is advantageous in many tasks.                |           Using ChatGPT could bring personal disadvantages.| 97|        0.09|  -0.11|    0.28| 95|  0.87| 0.385|      |
      |Using ChatGPT is advantageous in many tasks.                |               In my environment, using ChatGPT is standard.| 97|        0.33|   0.14|    0.50| 95|  3.46| 0.001|   ***|
      |Using ChatGPT is advantageous in many tasks.                |             Almost everyone in my environment uses ChatGPT.| 97|        0.34|   0.15|    0.50| 95|  3.51| 0.001|   ***|
      |Using ChatGPT is advantageous in many tasks.                |          Not using ChatGPT is considered being an outsider.| 97|        0.13|  -0.07|    0.32| 95|  1.31| 0.195|      |
      |Using ChatGPT is advantageous in many tasks.                |    Using ChatGPT brings me recognition from my environment.| 97|        0.31|   0.12|    0.48| 95|  3.18| 0.002|    **|
      |Compared to other systems, using ChatGPT is more fun.       | ChatGPT has clear advantages compared to similar offerings.| 97|        0.61|   0.47|    0.72| 95|  7.47| 0.000|   ***|
      |Compared to other systems, using ChatGPT is more fun.       |                    Using ChatGPT brings financial benefits.| 97|        0.42|   0.24|    0.57| 95|  4.55| 0.000|   ***|
      |Compared to other systems, using ChatGPT is more fun.       |                Using ChatGPT is advantageous in many tasks.| 97|        0.47|   0.30|    0.61| 95|  5.18| 0.000|   ***|
      |Compared to other systems, using ChatGPT is more fun.       |       Compared to other systems, using ChatGPT is more fun.| 97|        1.00|   1.00|    1.00| 95|   Inf| 0.000|   ***|
      |Compared to other systems, using ChatGPT is more fun.       |                       Much can go wrong when using ChatGPT.| 97|       -0.19|  -0.38|    0.01| 95| -1.89| 0.062|     .|
      |Compared to other systems, using ChatGPT is more fun.       |                  There are legal issues with using ChatGPT.| 97|        0.17|  -0.03|    0.36| 95|  1.66| 0.100|      |
      |Compared to other systems, using ChatGPT is more fun.       |   The security of user data is not guaranteed with ChatGPT.| 97|        0.14|  -0.06|    0.33| 95|  1.40| 0.164|      |
      |Compared to other systems, using ChatGPT is more fun.       |           Using ChatGPT could bring personal disadvantages.| 97|       -0.07|  -0.26|    0.13| 95| -0.66| 0.513|      |
      |Compared to other systems, using ChatGPT is more fun.       |               In my environment, using ChatGPT is standard.| 97|        0.33|   0.14|    0.50| 95|  3.40| 0.001|   ***|
      |Compared to other systems, using ChatGPT is more fun.       |             Almost everyone in my environment uses ChatGPT.| 97|        0.26|   0.06|    0.43| 95|  2.60| 0.011|     *|
      |Compared to other systems, using ChatGPT is more fun.       |          Not using ChatGPT is considered being an outsider.| 97|        0.20|   0.00|    0.38| 95|  1.94| 0.055|     .|
      |Compared to other systems, using ChatGPT is more fun.       |    Using ChatGPT brings me recognition from my environment.| 97|        0.36|   0.17|    0.52| 95|  3.72| 0.000|   ***|
      |Much can go wrong when using ChatGPT.                       | ChatGPT has clear advantages compared to similar offerings.| 97|       -0.14|  -0.33|    0.06| 95| -1.40| 0.166|      |
      |Much can go wrong when using ChatGPT.                       |                    Using ChatGPT brings financial benefits.| 97|        0.02|  -0.18|    0.22| 95|  0.22| 0.830|      |
      |Much can go wrong when using ChatGPT.                       |                Using ChatGPT is advantageous in many tasks.| 97|       -0.11|  -0.30|    0.09| 95| -1.10| 0.274|      |
      |Much can go wrong when using ChatGPT.                       |       Compared to other systems, using ChatGPT is more fun.| 97|       -0.19|  -0.38|    0.01| 95| -1.89| 0.062|     .|
      |Much can go wrong when using ChatGPT.                       |                       Much can go wrong when using ChatGPT.| 97|        1.00|   1.00|    1.00| 95|   Inf| 0.000|   ***|
      |Much can go wrong when using ChatGPT.                       |                  There are legal issues with using ChatGPT.| 97|        0.29|   0.09|    0.46| 95|  2.92| 0.004|    **|
      |Much can go wrong when using ChatGPT.                       |   The security of user data is not guaranteed with ChatGPT.| 97|        0.36|   0.17|    0.52| 95|  3.74| 0.000|   ***|
      |Much can go wrong when using ChatGPT.                       |           Using ChatGPT could bring personal disadvantages.| 97|        0.48|   0.31|    0.62| 95|  5.34| 0.000|   ***|
      |Much can go wrong when using ChatGPT.                       |               In my environment, using ChatGPT is standard.| 97|       -0.19|  -0.37|    0.01| 95| -1.85| 0.068|     .|
      |Much can go wrong when using ChatGPT.                       |             Almost everyone in my environment uses ChatGPT.| 97|        0.04|  -0.16|    0.23| 95|  0.35| 0.725|      |
      |Much can go wrong when using ChatGPT.                       |          Not using ChatGPT is considered being an outsider.| 97|        0.10|  -0.11|    0.29| 95|  0.94| 0.350|      |
      |Much can go wrong when using ChatGPT.                       |    Using ChatGPT brings me recognition from my environment.| 97|       -0.07|  -0.27|    0.13| 95| -0.73| 0.470|      |
      |There are legal issues with using ChatGPT.                  | ChatGPT has clear advantages compared to similar offerings.| 97|        0.19|  -0.01|    0.38| 95|  1.90| 0.061|     .|
      |There are legal issues with using ChatGPT.                  |                    Using ChatGPT brings financial benefits.| 97|        0.34|   0.15|    0.50| 95|  3.52| 0.001|   ***|
      |There are legal issues with using ChatGPT.                  |                Using ChatGPT is advantageous in many tasks.| 97|        0.19|  -0.01|    0.38| 95|  1.93| 0.056|     .|
      |There are legal issues with using ChatGPT.                  |       Compared to other systems, using ChatGPT is more fun.| 97|        0.17|  -0.03|    0.36| 95|  1.66| 0.100|      |
      |There are legal issues with using ChatGPT.                  |                       Much can go wrong when using ChatGPT.| 97|        0.29|   0.09|    0.46| 95|  2.92| 0.004|    **|
      |There are legal issues with using ChatGPT.                  |                  There are legal issues with using ChatGPT.| 97|        1.00|   1.00|    1.00| 95|   Inf| 0.000|   ***|
      |There are legal issues with using ChatGPT.                  |   The security of user data is not guaranteed with ChatGPT.| 97|        0.38|   0.20|    0.54| 95|  4.06| 0.000|   ***|
      |There are legal issues with using ChatGPT.                  |           Using ChatGPT could bring personal disadvantages.| 97|        0.28|   0.08|    0.45| 95|  2.83| 0.006|    **|
      |There are legal issues with using ChatGPT.                  |               In my environment, using ChatGPT is standard.| 97|        0.23|   0.03|    0.41| 95|  2.28| 0.025|     *|
      |There are legal issues with using ChatGPT.                  |             Almost everyone in my environment uses ChatGPT.| 97|        0.22|   0.02|    0.40| 95|  2.18| 0.031|     *|
      |There are legal issues with using ChatGPT.                  |          Not using ChatGPT is considered being an outsider.| 97|        0.33|   0.15|    0.50| 95|  3.46| 0.001|   ***|
      |There are legal issues with using ChatGPT.                  |    Using ChatGPT brings me recognition from my environment.| 97|        0.30|   0.11|    0.47| 95|  3.08| 0.003|    **|
      |The security of user data is not guaranteed with ChatGPT.   | ChatGPT has clear advantages compared to similar offerings.| 97|        0.07|  -0.13|    0.27| 95|  0.71| 0.477|      |
      |The security of user data is not guaranteed with ChatGPT.   |                    Using ChatGPT brings financial benefits.| 97|        0.34|   0.15|    0.51| 95|  3.56| 0.001|   ***|
      |The security of user data is not guaranteed with ChatGPT.   |                Using ChatGPT is advantageous in many tasks.| 97|        0.06|  -0.14|    0.25| 95|  0.57| 0.573|      |
      |The security of user data is not guaranteed with ChatGPT.   |       Compared to other systems, using ChatGPT is more fun.| 97|        0.14|  -0.06|    0.33| 95|  1.40| 0.164|      |
      |The security of user data is not guaranteed with ChatGPT.   |                       Much can go wrong when using ChatGPT.| 97|        0.36|   0.17|    0.52| 95|  3.74| 0.000|   ***|
      |The security of user data is not guaranteed with ChatGPT.   |                  There are legal issues with using ChatGPT.| 97|        0.38|   0.20|    0.54| 95|  4.06| 0.000|   ***|
      |The security of user data is not guaranteed with ChatGPT.   |   The security of user data is not guaranteed with ChatGPT.| 97|        1.00|   1.00|    1.00| 95|   Inf| 0.000|   ***|
      |The security of user data is not guaranteed with ChatGPT.   |           Using ChatGPT could bring personal disadvantages.| 97|        0.39|   0.21|    0.55| 95|  4.14| 0.000|   ***|
      |The security of user data is not guaranteed with ChatGPT.   |               In my environment, using ChatGPT is standard.| 97|        0.06|  -0.14|    0.26| 95|  0.61| 0.544|      |
      |The security of user data is not guaranteed with ChatGPT.   |             Almost everyone in my environment uses ChatGPT.| 97|        0.20|   0.00|    0.38| 95|  1.94| 0.055|     .|
      |The security of user data is not guaranteed with ChatGPT.   |          Not using ChatGPT is considered being an outsider.| 97|        0.29|   0.09|    0.46| 95|  2.91| 0.005|    **|
      |The security of user data is not guaranteed with ChatGPT.   |    Using ChatGPT brings me recognition from my environment.| 97|        0.10|  -0.11|    0.29| 95|  0.93| 0.354|      |
      |Using ChatGPT could bring personal disadvantages.           | ChatGPT has clear advantages compared to similar offerings.| 97|        0.01|  -0.19|    0.21| 95|  0.10| 0.924|      |
      |Using ChatGPT could bring personal disadvantages.           |                    Using ChatGPT brings financial benefits.| 97|        0.22|   0.02|    0.40| 95|  2.20| 0.030|     *|
      |Using ChatGPT could bring personal disadvantages.           |                Using ChatGPT is advantageous in many tasks.| 97|        0.09|  -0.11|    0.28| 95|  0.87| 0.385|      |
      |Using ChatGPT could bring personal disadvantages.           |       Compared to other systems, using ChatGPT is more fun.| 97|       -0.07|  -0.26|    0.13| 95| -0.66| 0.513|      |
      |Using ChatGPT could bring personal disadvantages.           |                       Much can go wrong when using ChatGPT.| 97|        0.48|   0.31|    0.62| 95|  5.34| 0.000|   ***|
      |Using ChatGPT could bring personal disadvantages.           |                  There are legal issues with using ChatGPT.| 97|        0.28|   0.08|    0.45| 95|  2.83| 0.006|    **|
      |Using ChatGPT could bring personal disadvantages.           |   The security of user data is not guaranteed with ChatGPT.| 97|        0.39|   0.21|    0.55| 95|  4.14| 0.000|   ***|
      |Using ChatGPT could bring personal disadvantages.           |           Using ChatGPT could bring personal disadvantages.| 97|        1.00|   1.00|    1.00| 95|   Inf| 0.000|   ***|
      |Using ChatGPT could bring personal disadvantages.           |               In my environment, using ChatGPT is standard.| 97|        0.25|   0.06|    0.43| 95|  2.54| 0.013|     *|
      |Using ChatGPT could bring personal disadvantages.           |             Almost everyone in my environment uses ChatGPT.| 97|        0.22|   0.02|    0.40| 95|  2.23| 0.028|     *|
      |Using ChatGPT could bring personal disadvantages.           |          Not using ChatGPT is considered being an outsider.| 97|        0.25|   0.06|    0.43| 95|  2.56| 0.012|     *|
      |Using ChatGPT could bring personal disadvantages.           |    Using ChatGPT brings me recognition from my environment.| 97|        0.14|  -0.06|    0.33| 95|  1.37| 0.174|      |
      |In my environment, using ChatGPT is standard.               | ChatGPT has clear advantages compared to similar offerings.| 97|        0.21|   0.01|    0.39| 95|  2.07| 0.041|     *|
      |In my environment, using ChatGPT is standard.               |                    Using ChatGPT brings financial benefits.| 97|        0.54|   0.38|    0.67| 95|  6.24| 0.000|   ***|
      |In my environment, using ChatGPT is standard.               |                Using ChatGPT is advantageous in many tasks.| 97|        0.33|   0.14|    0.50| 95|  3.46| 0.001|   ***|
      |In my environment, using ChatGPT is standard.               |       Compared to other systems, using ChatGPT is more fun.| 97|        0.33|   0.14|    0.50| 95|  3.40| 0.001|   ***|
      |In my environment, using ChatGPT is standard.               |                       Much can go wrong when using ChatGPT.| 97|       -0.19|  -0.37|    0.01| 95| -1.85| 0.068|     .|
      |In my environment, using ChatGPT is standard.               |                  There are legal issues with using ChatGPT.| 97|        0.23|   0.03|    0.41| 95|  2.28| 0.025|     *|
      |In my environment, using ChatGPT is standard.               |   The security of user data is not guaranteed with ChatGPT.| 97|        0.06|  -0.14|    0.26| 95|  0.61| 0.544|      |
      |In my environment, using ChatGPT is standard.               |           Using ChatGPT could bring personal disadvantages.| 97|        0.25|   0.06|    0.43| 95|  2.54| 0.013|     *|
      |In my environment, using ChatGPT is standard.               |               In my environment, using ChatGPT is standard.| 97|        1.00|   1.00|    1.00| 95|   Inf| 0.000|   ***|
      |In my environment, using ChatGPT is standard.               |             Almost everyone in my environment uses ChatGPT.| 97|        0.73|   0.62|    0.81| 95| 10.47| 0.000|   ***|
      |In my environment, using ChatGPT is standard.               |          Not using ChatGPT is considered being an outsider.| 97|        0.48|   0.31|    0.62| 95|  5.34| 0.000|   ***|
      |In my environment, using ChatGPT is standard.               |    Using ChatGPT brings me recognition from my environment.| 97|        0.57|   0.42|    0.69| 95|  6.85| 0.000|   ***|
      |Almost everyone in my environment uses ChatGPT.             | ChatGPT has clear advantages compared to similar offerings.| 97|        0.28|   0.08|    0.45| 95|  2.84| 0.006|    **|
      |Almost everyone in my environment uses ChatGPT.             |                    Using ChatGPT brings financial benefits.| 97|        0.50|   0.34|    0.64| 95|  5.67| 0.000|   ***|
      |Almost everyone in my environment uses ChatGPT.             |                Using ChatGPT is advantageous in many tasks.| 97|        0.34|   0.15|    0.50| 95|  3.51| 0.001|   ***|
      |Almost everyone in my environment uses ChatGPT.             |       Compared to other systems, using ChatGPT is more fun.| 97|        0.26|   0.06|    0.43| 95|  2.60| 0.011|     *|
      |Almost everyone in my environment uses ChatGPT.             |                       Much can go wrong when using ChatGPT.| 97|        0.04|  -0.16|    0.23| 95|  0.35| 0.725|      |
      |Almost everyone in my environment uses ChatGPT.             |                  There are legal issues with using ChatGPT.| 97|        0.22|   0.02|    0.40| 95|  2.18| 0.031|     *|
      |Almost everyone in my environment uses ChatGPT.             |   The security of user data is not guaranteed with ChatGPT.| 97|        0.20|   0.00|    0.38| 95|  1.94| 0.055|     .|
      |Almost everyone in my environment uses ChatGPT.             |           Using ChatGPT could bring personal disadvantages.| 97|        0.22|   0.02|    0.40| 95|  2.23| 0.028|     *|
      |Almost everyone in my environment uses ChatGPT.             |               In my environment, using ChatGPT is standard.| 97|        0.73|   0.62|    0.81| 95| 10.47| 0.000|   ***|
      |Almost everyone in my environment uses ChatGPT.             |             Almost everyone in my environment uses ChatGPT.| 97|        1.00|   1.00|    1.00| 95|   Inf| 0.000|   ***|
      |Almost everyone in my environment uses ChatGPT.             |          Not using ChatGPT is considered being an outsider.| 97|        0.58|   0.43|    0.70| 95|  6.88| 0.000|   ***|
      |Almost everyone in my environment uses ChatGPT.             |    Using ChatGPT brings me recognition from my environment.| 97|        0.54|   0.38|    0.67| 95|  6.23| 0.000|   ***|
      |Not using ChatGPT is considered being an outsider.          | ChatGPT has clear advantages compared to similar offerings.| 97|        0.16|  -0.04|    0.35| 95|  1.59| 0.115|      |
      |Not using ChatGPT is considered being an outsider.          |                    Using ChatGPT brings financial benefits.| 97|        0.36|   0.18|    0.53| 95|  3.81| 0.000|   ***|
      |Not using ChatGPT is considered being an outsider.          |                Using ChatGPT is advantageous in many tasks.| 97|        0.13|  -0.07|    0.32| 95|  1.31| 0.195|      |
      |Not using ChatGPT is considered being an outsider.          |       Compared to other systems, using ChatGPT is more fun.| 97|        0.20|   0.00|    0.38| 95|  1.94| 0.055|     .|
      |Not using ChatGPT is considered being an outsider.          |                       Much can go wrong when using ChatGPT.| 97|        0.10|  -0.11|    0.29| 95|  0.94| 0.350|      |
      |Not using ChatGPT is considered being an outsider.          |                  There are legal issues with using ChatGPT.| 97|        0.33|   0.15|    0.50| 95|  3.46| 0.001|   ***|
      |Not using ChatGPT is considered being an outsider.          |   The security of user data is not guaranteed with ChatGPT.| 97|        0.29|   0.09|    0.46| 95|  2.91| 0.005|    **|
      |Not using ChatGPT is considered being an outsider.          |           Using ChatGPT could bring personal disadvantages.| 97|        0.25|   0.06|    0.43| 95|  2.56| 0.012|     *|
      |Not using ChatGPT is considered being an outsider.          |               In my environment, using ChatGPT is standard.| 97|        0.48|   0.31|    0.62| 95|  5.34| 0.000|   ***|
      |Not using ChatGPT is considered being an outsider.          |             Almost everyone in my environment uses ChatGPT.| 97|        0.58|   0.43|    0.70| 95|  6.88| 0.000|   ***|
      |Not using ChatGPT is considered being an outsider.          |          Not using ChatGPT is considered being an outsider.| 97|        1.00|   1.00|    1.00| 95|   Inf| 0.000|   ***|
      |Not using ChatGPT is considered being an outsider.          |    Using ChatGPT brings me recognition from my environment.| 97|        0.56|   0.40|    0.68| 95|  6.53| 0.000|   ***|
      |Using ChatGPT brings me recognition from my environment.    | ChatGPT has clear advantages compared to similar offerings.| 97|        0.27|   0.07|    0.44| 95|  2.68| 0.009|    **|
      |Using ChatGPT brings me recognition from my environment.    |                    Using ChatGPT brings financial benefits.| 97|        0.40|   0.22|    0.55| 95|  4.25| 0.000|   ***|
      |Using ChatGPT brings me recognition from my environment.    |                Using ChatGPT is advantageous in many tasks.| 97|        0.31|   0.12|    0.48| 95|  3.18| 0.002|    **|
      |Using ChatGPT brings me recognition from my environment.    |       Compared to other systems, using ChatGPT is more fun.| 97|        0.36|   0.17|    0.52| 95|  3.72| 0.000|   ***|
      |Using ChatGPT brings me recognition from my environment.    |                       Much can go wrong when using ChatGPT.| 97|       -0.07|  -0.27|    0.13| 95| -0.73| 0.470|      |
      |Using ChatGPT brings me recognition from my environment.    |                  There are legal issues with using ChatGPT.| 97|        0.30|   0.11|    0.47| 95|  3.08| 0.003|    **|
      |Using ChatGPT brings me recognition from my environment.    |   The security of user data is not guaranteed with ChatGPT.| 97|        0.10|  -0.11|    0.29| 95|  0.93| 0.354|      |
      |Using ChatGPT brings me recognition from my environment.    |           Using ChatGPT could bring personal disadvantages.| 97|        0.14|  -0.06|    0.33| 95|  1.37| 0.174|      |
      |Using ChatGPT brings me recognition from my environment.    |               In my environment, using ChatGPT is standard.| 97|        0.57|   0.42|    0.69| 95|  6.85| 0.000|   ***|
      |Using ChatGPT brings me recognition from my environment.    |             Almost everyone in my environment uses ChatGPT.| 97|        0.54|   0.38|    0.67| 95|  6.23| 0.000|   ***|
      |Using ChatGPT brings me recognition from my environment.    |          Not using ChatGPT is considered being an outsider.| 97|        0.56|   0.40|    0.68| 95|  6.53| 0.000|   ***|
      |Using ChatGPT brings me recognition from my environment.    |    Using ChatGPT brings me recognition from my environment.| 97|        1.00|   1.00|    1.00| 95|   Inf| 0.000|   ***|
      
      5 missing case(s) ommited.
      

# effect_metrics_items_cor with two batteries

    Code
      volker::effect_metrics(data, tidyselect::starts_with("cg_adoption_"),
      tidyselect::starts_with("use_"), metric = TRUE)
    Output
      
      
      |Item 1                                                                    |                         Item 2|  n| Pearson's r| ci.low| ci.high| df|     t|     p| stars|
      |:-------------------------------------------------------------------------|------------------------------:|--:|-----------:|------:|-------:|--:|-----:|-----:|-----:|
      |Expectations: ChatGPT has clear advantages compared to similar offerings. |      Usage: in private context| 97|        0.50|   0.33|    0.63| 95|  5.60| 0.000|   ***|
      |Expectations: ChatGPT has clear advantages compared to similar offerings. | Usage: in professional context| 97|        0.28|   0.09|    0.46| 95|  2.86| 0.005|    **|
      |Expectations: Using ChatGPT brings financial benefits.                    |      Usage: in private context| 97|        0.21|   0.01|    0.39| 95|  2.04| 0.044|     *|
      |Expectations: Using ChatGPT brings financial benefits.                    | Usage: in professional context| 97|        0.54|   0.39|    0.67| 95|  6.31| 0.000|   ***|
      |Expectations: Using ChatGPT is advantageous in many tasks.                |      Usage: in private context| 97|        0.37|   0.18|    0.53| 95|  3.83| 0.000|   ***|
      |Expectations: Using ChatGPT is advantageous in many tasks.                | Usage: in professional context| 97|        0.37|   0.18|    0.53| 95|  3.86| 0.000|   ***|
      |Expectations: Compared to other systems, using ChatGPT is more fun.       |      Usage: in private context| 97|        0.47|   0.29|    0.61| 95|  5.13| 0.000|   ***|
      |Expectations: Compared to other systems, using ChatGPT is more fun.       | Usage: in professional context| 97|        0.29|   0.10|    0.47| 95|  2.99| 0.004|    **|
      |Expectations: Much can go wrong when using ChatGPT.                       |      Usage: in private context| 97|       -0.24|  -0.42|   -0.04| 95| -2.41| 0.018|     *|
      |Expectations: Much can go wrong when using ChatGPT.                       | Usage: in professional context| 97|       -0.09|  -0.28|    0.11| 95| -0.89| 0.378|      |
      |Expectations: There are legal issues with using ChatGPT.                  |      Usage: in private context| 97|        0.08|  -0.12|    0.28| 95|  0.82| 0.413|      |
      |Expectations: There are legal issues with using ChatGPT.                  | Usage: in professional context| 97|        0.31|   0.12|    0.48| 95|  3.17| 0.002|    **|
      |Expectations: The security of user data is not guaranteed with ChatGPT.   |      Usage: in private context| 97|        0.02|  -0.18|    0.22| 95|  0.20| 0.844|      |
      |Expectations: The security of user data is not guaranteed with ChatGPT.   | Usage: in professional context| 97|        0.20|   0.00|    0.39| 95|  2.01| 0.047|     *|
      |Expectations: Using ChatGPT could bring personal disadvantages.           |      Usage: in private context| 97|       -0.09|  -0.28|    0.11| 95| -0.88| 0.382|      |
      |Expectations: Using ChatGPT could bring personal disadvantages.           | Usage: in professional context| 97|        0.15|  -0.05|    0.34| 95|  1.49| 0.140|      |
      |Expectations: In my environment, using ChatGPT is standard.               |      Usage: in private context| 97|        0.40|   0.22|    0.55| 95|  4.24| 0.000|   ***|
      |Expectations: In my environment, using ChatGPT is standard.               | Usage: in professional context| 97|        0.58|   0.43|    0.70| 95|  6.96| 0.000|   ***|
      |Expectations: Almost everyone in my environment uses ChatGPT.             |      Usage: in private context| 97|        0.47|   0.30|    0.62| 95|  5.25| 0.000|   ***|
      |Expectations: Almost everyone in my environment uses ChatGPT.             | Usage: in professional context| 97|        0.55|   0.39|    0.67| 95|  6.37| 0.000|   ***|
      |Expectations: Not using ChatGPT is considered being an outsider.          |      Usage: in private context| 97|        0.34|   0.15|    0.50| 95|  3.48| 0.001|   ***|
      |Expectations: Not using ChatGPT is considered being an outsider.          | Usage: in professional context| 97|        0.34|   0.15|    0.51| 95|  3.55| 0.001|   ***|
      |Expectations: Using ChatGPT brings me recognition from my environment.    |      Usage: in private context| 97|        0.42|   0.24|    0.57| 95|  4.52| 0.000|   ***|
      |Expectations: Using ChatGPT brings me recognition from my environment.    | Usage: in professional context| 97|        0.46|   0.28|    0.60| 95|  4.99| 0.000|   ***|
      
      5 missing case(s) ommited.
      

