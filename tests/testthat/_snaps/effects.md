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
      |Phi                |  0.05|
      |Cramer's V         |  0.05|
      |Chi-squared        |   0.2|
      |Degrees of freedom |     1|
      |p value            | 0.651|
      |stars              |      |

# effect_metrics_one_grouped

    Code
      volker::effect_metrics(data, sd_age, adopter)
    Condition
      Warning:
      There was 1 warning in `dplyr::arrange()`.
      i In argument: `..1 = tidyselect::all_of("Statistic")`.
      Caused by warning:
      ! Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
    Output
      
      
      |Term                                              | estimate| conf.low| conf.high| std.error|     t|    p| stars|
      |:-------------------------------------------------|--------:|--------:|---------:|---------:|-----:|----:|-----:|
      |(Intercept)                                       |    36.60|    29.55|     43.65|      3.55| 10.30| 0.00|   ***|
      |I try new offers immediately (Reference)          |         |         |          |          |      |     |      |
      |I try new offers rather quickly                   |     4.21|    -3.64|     12.06|      3.95|  1.06| 0.29|      |
      |I wait until offers establish themselves          |     1.17|    -7.97|     10.32|      4.61|  0.25| 0.80|      |
      |I only use new offers when I have no other choice |    19.40|    -8.81|     47.61|     14.21|  1.36| 0.17|      |
      
      
      |Statistic                     | value|
      |:-----------------------------|-----:|
      |Adjusted R squared            |     0|
      |Degrees of freedom            |     1|
      |p                             |   0.4|
      |Residuals' degrees of freedom |     3|
      |F                             |    97|
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
      |p           | 0.063|
      |stars       |     .|

# effect_metrics_items

    Code
      volker::effect_metrics(data, tidyselect::starts_with("use_"))
    Output
      
      
      |Item 1                  |                  Item 2|   n|    r| ci.low| ci.high| df|  p| stars|
      |:-----------------------|-----------------------:|---:|----:|------:|-------:|--:|--:|-----:|
      |in private context      | in professional context| 101| 0.37|   0.19|    0.53| 99|  0|   ***|
      |in professional context |      in private context| 101| 0.37|   0.19|    0.53| 99|  0|   ***|

# effect_metrics_items_cor with one variable

    Code
      volker::effect_metrics(data, tidyselect::starts_with("cg_adoption_"), sd_age,
      metric = TRUE)
    Output
      
      
      |Item 1                                                                    | Item 2|  n| Pearson's r| ci.low| ci.high| df|    p| stars|
      |:-------------------------------------------------------------------------|------:|--:|-----------:|------:|-------:|--:|----:|-----:|
      |Expectations: ChatGPT has clear advantages compared to similar offerings. |    Age| 97|       -0.12|  -0.32|    0.08| 95| 0.23|      |
      |Expectations: Using ChatGPT brings financial benefits.                    |    Age| 97|       -0.13|  -0.32|    0.07| 95| 0.20|      |
      |Expectations: Using ChatGPT is advantageous in many tasks.                |    Age| 97|       -0.09|  -0.28|    0.11| 95| 0.39|      |
      |Expectations: Compared to other systems, using ChatGPT is more fun.       |    Age| 97|       -0.12|  -0.31|    0.08| 95| 0.25|      |
      |Expectations: Much can go wrong when using ChatGPT.                       |    Age| 97|        0.18|  -0.02|    0.36| 95| 0.08|     .|
      |Expectations: There are legal issues with using ChatGPT.                  |    Age| 97|        0.24|   0.05|    0.42| 95| 0.02|     *|
      |Expectations: The security of user data is not guaranteed with ChatGPT.   |    Age| 97|       -0.10|  -0.29|    0.11| 95| 0.35|      |
      |Expectations: Using ChatGPT could bring personal disadvantages.           |    Age| 97|        0.02|  -0.18|    0.22| 95| 0.85|      |
      |Expectations: In my environment, using ChatGPT is standard.               |    Age| 97|       -0.17|  -0.36|    0.03| 95| 0.09|     .|
      |Expectations: Almost everyone in my environment uses ChatGPT.             |    Age| 97|       -0.06|  -0.26|    0.14| 95| 0.56|      |
      |Expectations: Not using ChatGPT is considered being an outsider.          |    Age| 97|        0.09|  -0.11|    0.28| 95| 0.39|      |
      |Expectations: Using ChatGPT brings me recognition from my environment.    |    Age| 97|       -0.02|  -0.22|    0.18| 95| 0.82|      |
      
      5 missing case(s) ommited.
      

# effect_metrics_items_cor with items

    Code
      volker::effect_metrics(data, tidyselect::starts_with("cg_adoption_"),
      tidyselect::starts_with("cg_adoption_"), metric = TRUE)
    Output
      
      
      |Item 1                                                      |                                                      Item 2|  n| Pearson's r| ci.low| ci.high| df|    p| stars|
      |:-----------------------------------------------------------|-----------------------------------------------------------:|--:|-----------:|------:|-------:|--:|----:|-----:|
      |ChatGPT has clear advantages compared to similar offerings. | ChatGPT has clear advantages compared to similar offerings.| 97|        1.00|   1.00|    1.00| 95| 0.00|   ***|
      |ChatGPT has clear advantages compared to similar offerings. |                    Using ChatGPT brings financial benefits.| 97|        0.37|   0.19|    0.53| 95| 0.00|   ***|
      |ChatGPT has clear advantages compared to similar offerings. |                Using ChatGPT is advantageous in many tasks.| 97|        0.64|   0.50|    0.74| 95| 0.00|   ***|
      |ChatGPT has clear advantages compared to similar offerings. |       Compared to other systems, using ChatGPT is more fun.| 97|        0.61|   0.47|    0.72| 95| 0.00|   ***|
      |ChatGPT has clear advantages compared to similar offerings. |                       Much can go wrong when using ChatGPT.| 97|       -0.14|  -0.33|    0.06| 95| 0.17|      |
      |ChatGPT has clear advantages compared to similar offerings. |                  There are legal issues with using ChatGPT.| 97|        0.19|  -0.01|    0.38| 95| 0.06|     .|
      |ChatGPT has clear advantages compared to similar offerings. |   The security of user data is not guaranteed with ChatGPT.| 97|        0.07|  -0.13|    0.27| 95| 0.48|      |
      |ChatGPT has clear advantages compared to similar offerings. |           Using ChatGPT could bring personal disadvantages.| 97|        0.01|  -0.19|    0.21| 95| 0.92|      |
      |ChatGPT has clear advantages compared to similar offerings. |               In my environment, using ChatGPT is standard.| 97|        0.21|   0.01|    0.39| 95| 0.04|     *|
      |ChatGPT has clear advantages compared to similar offerings. |             Almost everyone in my environment uses ChatGPT.| 97|        0.28|   0.08|    0.45| 95| 0.01|    **|
      |ChatGPT has clear advantages compared to similar offerings. |          Not using ChatGPT is considered being an outsider.| 97|        0.16|  -0.04|    0.35| 95| 0.12|      |
      |ChatGPT has clear advantages compared to similar offerings. |    Using ChatGPT brings me recognition from my environment.| 97|        0.27|   0.07|    0.44| 95| 0.01|    **|
      |Using ChatGPT brings financial benefits.                    | ChatGPT has clear advantages compared to similar offerings.| 97|        0.37|   0.19|    0.53| 95| 0.00|   ***|
      |Using ChatGPT brings financial benefits.                    |                    Using ChatGPT brings financial benefits.| 97|        1.00|   1.00|    1.00| 95| 0.00|   ***|
      |Using ChatGPT brings financial benefits.                    |                Using ChatGPT is advantageous in many tasks.| 97|        0.46|   0.29|    0.61| 95| 0.00|   ***|
      |Using ChatGPT brings financial benefits.                    |       Compared to other systems, using ChatGPT is more fun.| 97|        0.42|   0.24|    0.57| 95| 0.00|   ***|
      |Using ChatGPT brings financial benefits.                    |                       Much can go wrong when using ChatGPT.| 97|        0.02|  -0.18|    0.22| 95| 0.83|      |
      |Using ChatGPT brings financial benefits.                    |                  There are legal issues with using ChatGPT.| 97|        0.34|   0.15|    0.50| 95| 0.00|   ***|
      |Using ChatGPT brings financial benefits.                    |   The security of user data is not guaranteed with ChatGPT.| 97|        0.34|   0.15|    0.51| 95| 0.00|   ***|
      |Using ChatGPT brings financial benefits.                    |           Using ChatGPT could bring personal disadvantages.| 97|        0.22|   0.02|    0.40| 95| 0.03|     *|
      |Using ChatGPT brings financial benefits.                    |               In my environment, using ChatGPT is standard.| 97|        0.54|   0.38|    0.67| 95| 0.00|   ***|
      |Using ChatGPT brings financial benefits.                    |             Almost everyone in my environment uses ChatGPT.| 97|        0.50|   0.34|    0.64| 95| 0.00|   ***|
      |Using ChatGPT brings financial benefits.                    |          Not using ChatGPT is considered being an outsider.| 97|        0.36|   0.18|    0.53| 95| 0.00|   ***|
      |Using ChatGPT brings financial benefits.                    |    Using ChatGPT brings me recognition from my environment.| 97|        0.40|   0.22|    0.55| 95| 0.00|   ***|
      |Using ChatGPT is advantageous in many tasks.                | ChatGPT has clear advantages compared to similar offerings.| 97|        0.64|   0.50|    0.74| 95| 0.00|   ***|
      |Using ChatGPT is advantageous in many tasks.                |                    Using ChatGPT brings financial benefits.| 97|        0.46|   0.29|    0.61| 95| 0.00|   ***|
      |Using ChatGPT is advantageous in many tasks.                |                Using ChatGPT is advantageous in many tasks.| 97|        1.00|   1.00|    1.00| 95| 0.00|   ***|
      |Using ChatGPT is advantageous in many tasks.                |       Compared to other systems, using ChatGPT is more fun.| 97|        0.47|   0.30|    0.61| 95| 0.00|   ***|
      |Using ChatGPT is advantageous in many tasks.                |                       Much can go wrong when using ChatGPT.| 97|       -0.11|  -0.30|    0.09| 95| 0.27|      |
      |Using ChatGPT is advantageous in many tasks.                |                  There are legal issues with using ChatGPT.| 97|        0.19|  -0.01|    0.38| 95| 0.06|     .|
      |Using ChatGPT is advantageous in many tasks.                |   The security of user data is not guaranteed with ChatGPT.| 97|        0.06|  -0.14|    0.25| 95| 0.57|      |
      |Using ChatGPT is advantageous in many tasks.                |           Using ChatGPT could bring personal disadvantages.| 97|        0.09|  -0.11|    0.28| 95| 0.38|      |
      |Using ChatGPT is advantageous in many tasks.                |               In my environment, using ChatGPT is standard.| 97|        0.33|   0.14|    0.50| 95| 0.00|   ***|
      |Using ChatGPT is advantageous in many tasks.                |             Almost everyone in my environment uses ChatGPT.| 97|        0.34|   0.15|    0.50| 95| 0.00|   ***|
      |Using ChatGPT is advantageous in many tasks.                |          Not using ChatGPT is considered being an outsider.| 97|        0.13|  -0.07|    0.32| 95| 0.20|      |
      |Using ChatGPT is advantageous in many tasks.                |    Using ChatGPT brings me recognition from my environment.| 97|        0.31|   0.12|    0.48| 95| 0.00|    **|
      |Compared to other systems, using ChatGPT is more fun.       | ChatGPT has clear advantages compared to similar offerings.| 97|        0.61|   0.47|    0.72| 95| 0.00|   ***|
      |Compared to other systems, using ChatGPT is more fun.       |                    Using ChatGPT brings financial benefits.| 97|        0.42|   0.24|    0.57| 95| 0.00|   ***|
      |Compared to other systems, using ChatGPT is more fun.       |                Using ChatGPT is advantageous in many tasks.| 97|        0.47|   0.30|    0.61| 95| 0.00|   ***|
      |Compared to other systems, using ChatGPT is more fun.       |       Compared to other systems, using ChatGPT is more fun.| 97|        1.00|   1.00|    1.00| 95| 0.00|   ***|
      |Compared to other systems, using ChatGPT is more fun.       |                       Much can go wrong when using ChatGPT.| 97|       -0.19|  -0.38|    0.01| 95| 0.06|     .|
      |Compared to other systems, using ChatGPT is more fun.       |                  There are legal issues with using ChatGPT.| 97|        0.17|  -0.03|    0.36| 95| 0.10|      |
      |Compared to other systems, using ChatGPT is more fun.       |   The security of user data is not guaranteed with ChatGPT.| 97|        0.14|  -0.06|    0.33| 95| 0.16|      |
      |Compared to other systems, using ChatGPT is more fun.       |           Using ChatGPT could bring personal disadvantages.| 97|       -0.07|  -0.26|    0.13| 95| 0.51|      |
      |Compared to other systems, using ChatGPT is more fun.       |               In my environment, using ChatGPT is standard.| 97|        0.33|   0.14|    0.50| 95| 0.00|   ***|
      |Compared to other systems, using ChatGPT is more fun.       |             Almost everyone in my environment uses ChatGPT.| 97|        0.26|   0.06|    0.43| 95| 0.01|     *|
      |Compared to other systems, using ChatGPT is more fun.       |          Not using ChatGPT is considered being an outsider.| 97|        0.20|   0.00|    0.38| 95| 0.06|     .|
      |Compared to other systems, using ChatGPT is more fun.       |    Using ChatGPT brings me recognition from my environment.| 97|        0.36|   0.17|    0.52| 95| 0.00|   ***|
      |Much can go wrong when using ChatGPT.                       | ChatGPT has clear advantages compared to similar offerings.| 97|       -0.14|  -0.33|    0.06| 95| 0.17|      |
      |Much can go wrong when using ChatGPT.                       |                    Using ChatGPT brings financial benefits.| 97|        0.02|  -0.18|    0.22| 95| 0.83|      |
      |Much can go wrong when using ChatGPT.                       |                Using ChatGPT is advantageous in many tasks.| 97|       -0.11|  -0.30|    0.09| 95| 0.27|      |
      |Much can go wrong when using ChatGPT.                       |       Compared to other systems, using ChatGPT is more fun.| 97|       -0.19|  -0.38|    0.01| 95| 0.06|     .|
      |Much can go wrong when using ChatGPT.                       |                       Much can go wrong when using ChatGPT.| 97|        1.00|   1.00|    1.00| 95| 0.00|   ***|
      |Much can go wrong when using ChatGPT.                       |                  There are legal issues with using ChatGPT.| 97|        0.29|   0.09|    0.46| 95| 0.00|    **|
      |Much can go wrong when using ChatGPT.                       |   The security of user data is not guaranteed with ChatGPT.| 97|        0.36|   0.17|    0.52| 95| 0.00|   ***|
      |Much can go wrong when using ChatGPT.                       |           Using ChatGPT could bring personal disadvantages.| 97|        0.48|   0.31|    0.62| 95| 0.00|   ***|
      |Much can go wrong when using ChatGPT.                       |               In my environment, using ChatGPT is standard.| 97|       -0.19|  -0.37|    0.01| 95| 0.07|     .|
      |Much can go wrong when using ChatGPT.                       |             Almost everyone in my environment uses ChatGPT.| 97|        0.04|  -0.16|    0.23| 95| 0.72|      |
      |Much can go wrong when using ChatGPT.                       |          Not using ChatGPT is considered being an outsider.| 97|        0.10|  -0.11|    0.29| 95| 0.35|      |
      |Much can go wrong when using ChatGPT.                       |    Using ChatGPT brings me recognition from my environment.| 97|       -0.07|  -0.27|    0.13| 95| 0.47|      |
      |There are legal issues with using ChatGPT.                  | ChatGPT has clear advantages compared to similar offerings.| 97|        0.19|  -0.01|    0.38| 95| 0.06|     .|
      |There are legal issues with using ChatGPT.                  |                    Using ChatGPT brings financial benefits.| 97|        0.34|   0.15|    0.50| 95| 0.00|   ***|
      |There are legal issues with using ChatGPT.                  |                Using ChatGPT is advantageous in many tasks.| 97|        0.19|  -0.01|    0.38| 95| 0.06|     .|
      |There are legal issues with using ChatGPT.                  |       Compared to other systems, using ChatGPT is more fun.| 97|        0.17|  -0.03|    0.36| 95| 0.10|      |
      |There are legal issues with using ChatGPT.                  |                       Much can go wrong when using ChatGPT.| 97|        0.29|   0.09|    0.46| 95| 0.00|    **|
      |There are legal issues with using ChatGPT.                  |                  There are legal issues with using ChatGPT.| 97|        1.00|   1.00|    1.00| 95| 0.00|   ***|
      |There are legal issues with using ChatGPT.                  |   The security of user data is not guaranteed with ChatGPT.| 97|        0.38|   0.20|    0.54| 95| 0.00|   ***|
      |There are legal issues with using ChatGPT.                  |           Using ChatGPT could bring personal disadvantages.| 97|        0.28|   0.08|    0.45| 95| 0.01|    **|
      |There are legal issues with using ChatGPT.                  |               In my environment, using ChatGPT is standard.| 97|        0.23|   0.03|    0.41| 95| 0.03|     *|
      |There are legal issues with using ChatGPT.                  |             Almost everyone in my environment uses ChatGPT.| 97|        0.22|   0.02|    0.40| 95| 0.03|     *|
      |There are legal issues with using ChatGPT.                  |          Not using ChatGPT is considered being an outsider.| 97|        0.33|   0.15|    0.50| 95| 0.00|   ***|
      |There are legal issues with using ChatGPT.                  |    Using ChatGPT brings me recognition from my environment.| 97|        0.30|   0.11|    0.47| 95| 0.00|    **|
      |The security of user data is not guaranteed with ChatGPT.   | ChatGPT has clear advantages compared to similar offerings.| 97|        0.07|  -0.13|    0.27| 95| 0.48|      |
      |The security of user data is not guaranteed with ChatGPT.   |                    Using ChatGPT brings financial benefits.| 97|        0.34|   0.15|    0.51| 95| 0.00|   ***|
      |The security of user data is not guaranteed with ChatGPT.   |                Using ChatGPT is advantageous in many tasks.| 97|        0.06|  -0.14|    0.25| 95| 0.57|      |
      |The security of user data is not guaranteed with ChatGPT.   |       Compared to other systems, using ChatGPT is more fun.| 97|        0.14|  -0.06|    0.33| 95| 0.16|      |
      |The security of user data is not guaranteed with ChatGPT.   |                       Much can go wrong when using ChatGPT.| 97|        0.36|   0.17|    0.52| 95| 0.00|   ***|
      |The security of user data is not guaranteed with ChatGPT.   |                  There are legal issues with using ChatGPT.| 97|        0.38|   0.20|    0.54| 95| 0.00|   ***|
      |The security of user data is not guaranteed with ChatGPT.   |   The security of user data is not guaranteed with ChatGPT.| 97|        1.00|   1.00|    1.00| 95| 0.00|   ***|
      |The security of user data is not guaranteed with ChatGPT.   |           Using ChatGPT could bring personal disadvantages.| 97|        0.39|   0.21|    0.55| 95| 0.00|   ***|
      |The security of user data is not guaranteed with ChatGPT.   |               In my environment, using ChatGPT is standard.| 97|        0.06|  -0.14|    0.26| 95| 0.54|      |
      |The security of user data is not guaranteed with ChatGPT.   |             Almost everyone in my environment uses ChatGPT.| 97|        0.20|   0.00|    0.38| 95| 0.06|     .|
      |The security of user data is not guaranteed with ChatGPT.   |          Not using ChatGPT is considered being an outsider.| 97|        0.29|   0.09|    0.46| 95| 0.00|    **|
      |The security of user data is not guaranteed with ChatGPT.   |    Using ChatGPT brings me recognition from my environment.| 97|        0.10|  -0.11|    0.29| 95| 0.35|      |
      |Using ChatGPT could bring personal disadvantages.           | ChatGPT has clear advantages compared to similar offerings.| 97|        0.01|  -0.19|    0.21| 95| 0.92|      |
      |Using ChatGPT could bring personal disadvantages.           |                    Using ChatGPT brings financial benefits.| 97|        0.22|   0.02|    0.40| 95| 0.03|     *|
      |Using ChatGPT could bring personal disadvantages.           |                Using ChatGPT is advantageous in many tasks.| 97|        0.09|  -0.11|    0.28| 95| 0.38|      |
      |Using ChatGPT could bring personal disadvantages.           |       Compared to other systems, using ChatGPT is more fun.| 97|       -0.07|  -0.26|    0.13| 95| 0.51|      |
      |Using ChatGPT could bring personal disadvantages.           |                       Much can go wrong when using ChatGPT.| 97|        0.48|   0.31|    0.62| 95| 0.00|   ***|
      |Using ChatGPT could bring personal disadvantages.           |                  There are legal issues with using ChatGPT.| 97|        0.28|   0.08|    0.45| 95| 0.01|    **|
      |Using ChatGPT could bring personal disadvantages.           |   The security of user data is not guaranteed with ChatGPT.| 97|        0.39|   0.21|    0.55| 95| 0.00|   ***|
      |Using ChatGPT could bring personal disadvantages.           |           Using ChatGPT could bring personal disadvantages.| 97|        1.00|   1.00|    1.00| 95| 0.00|   ***|
      |Using ChatGPT could bring personal disadvantages.           |               In my environment, using ChatGPT is standard.| 97|        0.25|   0.06|    0.43| 95| 0.01|     *|
      |Using ChatGPT could bring personal disadvantages.           |             Almost everyone in my environment uses ChatGPT.| 97|        0.22|   0.02|    0.40| 95| 0.03|     *|
      |Using ChatGPT could bring personal disadvantages.           |          Not using ChatGPT is considered being an outsider.| 97|        0.25|   0.06|    0.43| 95| 0.01|     *|
      |Using ChatGPT could bring personal disadvantages.           |    Using ChatGPT brings me recognition from my environment.| 97|        0.14|  -0.06|    0.33| 95| 0.17|      |
      |In my environment, using ChatGPT is standard.               | ChatGPT has clear advantages compared to similar offerings.| 97|        0.21|   0.01|    0.39| 95| 0.04|     *|
      |In my environment, using ChatGPT is standard.               |                    Using ChatGPT brings financial benefits.| 97|        0.54|   0.38|    0.67| 95| 0.00|   ***|
      |In my environment, using ChatGPT is standard.               |                Using ChatGPT is advantageous in many tasks.| 97|        0.33|   0.14|    0.50| 95| 0.00|   ***|
      |In my environment, using ChatGPT is standard.               |       Compared to other systems, using ChatGPT is more fun.| 97|        0.33|   0.14|    0.50| 95| 0.00|   ***|
      |In my environment, using ChatGPT is standard.               |                       Much can go wrong when using ChatGPT.| 97|       -0.19|  -0.37|    0.01| 95| 0.07|     .|
      |In my environment, using ChatGPT is standard.               |                  There are legal issues with using ChatGPT.| 97|        0.23|   0.03|    0.41| 95| 0.03|     *|
      |In my environment, using ChatGPT is standard.               |   The security of user data is not guaranteed with ChatGPT.| 97|        0.06|  -0.14|    0.26| 95| 0.54|      |
      |In my environment, using ChatGPT is standard.               |           Using ChatGPT could bring personal disadvantages.| 97|        0.25|   0.06|    0.43| 95| 0.01|     *|
      |In my environment, using ChatGPT is standard.               |               In my environment, using ChatGPT is standard.| 97|        1.00|   1.00|    1.00| 95| 0.00|   ***|
      |In my environment, using ChatGPT is standard.               |             Almost everyone in my environment uses ChatGPT.| 97|        0.73|   0.62|    0.81| 95| 0.00|   ***|
      |In my environment, using ChatGPT is standard.               |          Not using ChatGPT is considered being an outsider.| 97|        0.48|   0.31|    0.62| 95| 0.00|   ***|
      |In my environment, using ChatGPT is standard.               |    Using ChatGPT brings me recognition from my environment.| 97|        0.57|   0.42|    0.69| 95| 0.00|   ***|
      |Almost everyone in my environment uses ChatGPT.             | ChatGPT has clear advantages compared to similar offerings.| 97|        0.28|   0.08|    0.45| 95| 0.01|    **|
      |Almost everyone in my environment uses ChatGPT.             |                    Using ChatGPT brings financial benefits.| 97|        0.50|   0.34|    0.64| 95| 0.00|   ***|
      |Almost everyone in my environment uses ChatGPT.             |                Using ChatGPT is advantageous in many tasks.| 97|        0.34|   0.15|    0.50| 95| 0.00|   ***|
      |Almost everyone in my environment uses ChatGPT.             |       Compared to other systems, using ChatGPT is more fun.| 97|        0.26|   0.06|    0.43| 95| 0.01|     *|
      |Almost everyone in my environment uses ChatGPT.             |                       Much can go wrong when using ChatGPT.| 97|        0.04|  -0.16|    0.23| 95| 0.72|      |
      |Almost everyone in my environment uses ChatGPT.             |                  There are legal issues with using ChatGPT.| 97|        0.22|   0.02|    0.40| 95| 0.03|     *|
      |Almost everyone in my environment uses ChatGPT.             |   The security of user data is not guaranteed with ChatGPT.| 97|        0.20|   0.00|    0.38| 95| 0.06|     .|
      |Almost everyone in my environment uses ChatGPT.             |           Using ChatGPT could bring personal disadvantages.| 97|        0.22|   0.02|    0.40| 95| 0.03|     *|
      |Almost everyone in my environment uses ChatGPT.             |               In my environment, using ChatGPT is standard.| 97|        0.73|   0.62|    0.81| 95| 0.00|   ***|
      |Almost everyone in my environment uses ChatGPT.             |             Almost everyone in my environment uses ChatGPT.| 97|        1.00|   1.00|    1.00| 95| 0.00|   ***|
      |Almost everyone in my environment uses ChatGPT.             |          Not using ChatGPT is considered being an outsider.| 97|        0.58|   0.43|    0.70| 95| 0.00|   ***|
      |Almost everyone in my environment uses ChatGPT.             |    Using ChatGPT brings me recognition from my environment.| 97|        0.54|   0.38|    0.67| 95| 0.00|   ***|
      |Not using ChatGPT is considered being an outsider.          | ChatGPT has clear advantages compared to similar offerings.| 97|        0.16|  -0.04|    0.35| 95| 0.12|      |
      |Not using ChatGPT is considered being an outsider.          |                    Using ChatGPT brings financial benefits.| 97|        0.36|   0.18|    0.53| 95| 0.00|   ***|
      |Not using ChatGPT is considered being an outsider.          |                Using ChatGPT is advantageous in many tasks.| 97|        0.13|  -0.07|    0.32| 95| 0.20|      |
      |Not using ChatGPT is considered being an outsider.          |       Compared to other systems, using ChatGPT is more fun.| 97|        0.20|   0.00|    0.38| 95| 0.06|     .|
      |Not using ChatGPT is considered being an outsider.          |                       Much can go wrong when using ChatGPT.| 97|        0.10|  -0.11|    0.29| 95| 0.35|      |
      |Not using ChatGPT is considered being an outsider.          |                  There are legal issues with using ChatGPT.| 97|        0.33|   0.15|    0.50| 95| 0.00|   ***|
      |Not using ChatGPT is considered being an outsider.          |   The security of user data is not guaranteed with ChatGPT.| 97|        0.29|   0.09|    0.46| 95| 0.00|    **|
      |Not using ChatGPT is considered being an outsider.          |           Using ChatGPT could bring personal disadvantages.| 97|        0.25|   0.06|    0.43| 95| 0.01|     *|
      |Not using ChatGPT is considered being an outsider.          |               In my environment, using ChatGPT is standard.| 97|        0.48|   0.31|    0.62| 95| 0.00|   ***|
      |Not using ChatGPT is considered being an outsider.          |             Almost everyone in my environment uses ChatGPT.| 97|        0.58|   0.43|    0.70| 95| 0.00|   ***|
      |Not using ChatGPT is considered being an outsider.          |          Not using ChatGPT is considered being an outsider.| 97|        1.00|   1.00|    1.00| 95| 0.00|   ***|
      |Not using ChatGPT is considered being an outsider.          |    Using ChatGPT brings me recognition from my environment.| 97|        0.56|   0.40|    0.68| 95| 0.00|   ***|
      |Using ChatGPT brings me recognition from my environment.    | ChatGPT has clear advantages compared to similar offerings.| 97|        0.27|   0.07|    0.44| 95| 0.01|    **|
      |Using ChatGPT brings me recognition from my environment.    |                    Using ChatGPT brings financial benefits.| 97|        0.40|   0.22|    0.55| 95| 0.00|   ***|
      |Using ChatGPT brings me recognition from my environment.    |                Using ChatGPT is advantageous in many tasks.| 97|        0.31|   0.12|    0.48| 95| 0.00|    **|
      |Using ChatGPT brings me recognition from my environment.    |       Compared to other systems, using ChatGPT is more fun.| 97|        0.36|   0.17|    0.52| 95| 0.00|   ***|
      |Using ChatGPT brings me recognition from my environment.    |                       Much can go wrong when using ChatGPT.| 97|       -0.07|  -0.27|    0.13| 95| 0.47|      |
      |Using ChatGPT brings me recognition from my environment.    |                  There are legal issues with using ChatGPT.| 97|        0.30|   0.11|    0.47| 95| 0.00|    **|
      |Using ChatGPT brings me recognition from my environment.    |   The security of user data is not guaranteed with ChatGPT.| 97|        0.10|  -0.11|    0.29| 95| 0.35|      |
      |Using ChatGPT brings me recognition from my environment.    |           Using ChatGPT could bring personal disadvantages.| 97|        0.14|  -0.06|    0.33| 95| 0.17|      |
      |Using ChatGPT brings me recognition from my environment.    |               In my environment, using ChatGPT is standard.| 97|        0.57|   0.42|    0.69| 95| 0.00|   ***|
      |Using ChatGPT brings me recognition from my environment.    |             Almost everyone in my environment uses ChatGPT.| 97|        0.54|   0.38|    0.67| 95| 0.00|   ***|
      |Using ChatGPT brings me recognition from my environment.    |          Not using ChatGPT is considered being an outsider.| 97|        0.56|   0.40|    0.68| 95| 0.00|   ***|
      |Using ChatGPT brings me recognition from my environment.    |    Using ChatGPT brings me recognition from my environment.| 97|        1.00|   1.00|    1.00| 95| 0.00|   ***|
      
      5 missing case(s) ommited.
      

# effect_metrics_items_cor with two batteries

    Code
      volker::effect_metrics(data, tidyselect::starts_with("cg_adoption_"),
      tidyselect::starts_with("use_"), metric = TRUE)
    Output
      
      
      |Item 1                                                                    |                         Item 2|  n| Pearson's r| ci.low| ci.high| df|    p| stars|
      |:-------------------------------------------------------------------------|------------------------------:|--:|-----------:|------:|-------:|--:|----:|-----:|
      |Expectations: ChatGPT has clear advantages compared to similar offerings. |      Usage: in private context| 97|        0.50|   0.33|    0.63| 95| 0.00|   ***|
      |Expectations: ChatGPT has clear advantages compared to similar offerings. | Usage: in professional context| 97|        0.28|   0.09|    0.46| 95| 0.00|    **|
      |Expectations: Using ChatGPT brings financial benefits.                    |      Usage: in private context| 97|        0.21|   0.01|    0.39| 95| 0.04|     *|
      |Expectations: Using ChatGPT brings financial benefits.                    | Usage: in professional context| 97|        0.54|   0.39|    0.67| 95| 0.00|   ***|
      |Expectations: Using ChatGPT is advantageous in many tasks.                |      Usage: in private context| 97|        0.37|   0.18|    0.53| 95| 0.00|   ***|
      |Expectations: Using ChatGPT is advantageous in many tasks.                | Usage: in professional context| 97|        0.37|   0.18|    0.53| 95| 0.00|   ***|
      |Expectations: Compared to other systems, using ChatGPT is more fun.       |      Usage: in private context| 97|        0.47|   0.29|    0.61| 95| 0.00|   ***|
      |Expectations: Compared to other systems, using ChatGPT is more fun.       | Usage: in professional context| 97|        0.29|   0.10|    0.47| 95| 0.00|    **|
      |Expectations: Much can go wrong when using ChatGPT.                       |      Usage: in private context| 97|       -0.24|  -0.42|   -0.04| 95| 0.02|     *|
      |Expectations: Much can go wrong when using ChatGPT.                       | Usage: in professional context| 97|       -0.09|  -0.28|    0.11| 95| 0.38|      |
      |Expectations: There are legal issues with using ChatGPT.                  |      Usage: in private context| 97|        0.08|  -0.12|    0.28| 95| 0.41|      |
      |Expectations: There are legal issues with using ChatGPT.                  | Usage: in professional context| 97|        0.31|   0.12|    0.48| 95| 0.00|    **|
      |Expectations: The security of user data is not guaranteed with ChatGPT.   |      Usage: in private context| 97|        0.02|  -0.18|    0.22| 95| 0.84|      |
      |Expectations: The security of user data is not guaranteed with ChatGPT.   | Usage: in professional context| 97|        0.20|   0.00|    0.39| 95| 0.05|     *|
      |Expectations: Using ChatGPT could bring personal disadvantages.           |      Usage: in private context| 97|       -0.09|  -0.28|    0.11| 95| 0.38|      |
      |Expectations: Using ChatGPT could bring personal disadvantages.           | Usage: in professional context| 97|        0.15|  -0.05|    0.34| 95| 0.14|      |
      |Expectations: In my environment, using ChatGPT is standard.               |      Usage: in private context| 97|        0.40|   0.22|    0.55| 95| 0.00|   ***|
      |Expectations: In my environment, using ChatGPT is standard.               | Usage: in professional context| 97|        0.58|   0.43|    0.70| 95| 0.00|   ***|
      |Expectations: Almost everyone in my environment uses ChatGPT.             |      Usage: in private context| 97|        0.47|   0.30|    0.62| 95| 0.00|   ***|
      |Expectations: Almost everyone in my environment uses ChatGPT.             | Usage: in professional context| 97|        0.55|   0.39|    0.67| 95| 0.00|   ***|
      |Expectations: Not using ChatGPT is considered being an outsider.          |      Usage: in private context| 97|        0.34|   0.15|    0.50| 95| 0.00|   ***|
      |Expectations: Not using ChatGPT is considered being an outsider.          | Usage: in professional context| 97|        0.34|   0.15|    0.51| 95| 0.00|   ***|
      |Expectations: Using ChatGPT brings me recognition from my environment.    |      Usage: in private context| 97|        0.42|   0.24|    0.57| 95| 0.00|   ***|
      |Expectations: Using ChatGPT brings me recognition from my environment.    | Usage: in professional context| 97|        0.46|   0.28|    0.60| 95| 0.00|   ***|
      
      5 missing case(s) ommited.
      

