# Frequency table

    Code
      volker::tab_counts(data, sd_gender)
    Output
      
      
      |Gender  |   n|    p|
      |:-------|---:|----:|
      |female  |  40|  40%|
      |male    |  60|  59%|
      |diverse |   1|   1%|
      |total   | 101| 100%|

# Cross table of categorical variables

    Code
      volker::tab_counts(data, adopter, sd_gender)
    Output
      
      
      |Innovator type                                    |      total|   female|     male| diverse|
      |:-------------------------------------------------|----------:|--------:|--------:|-------:|
      |I try new offers immediately                      |   15% (15)|   2% (2)| 12% (12)|  1% (1)|
      |I try new offers rather quickly                   |   62% (63)| 25% (25)| 38% (38)|  0% (0)|
      |I wait until offers establish themselves          |   22% (22)| 13% (13)|   9% (9)|  0% (0)|
      |I only use new offers when I have no other choice |     1% (1)|   0% (0)|   1% (1)|  0% (0)|
      |total                                             | 100% (101)| 40% (40)| 59% (60)|  1% (1)|

# Frequency table for multiple categorical variables

    Code
      volker::tab_counts(data, tidyselect::starts_with("cg_adoption_"))
    Output
      
      
      |Expectations                                                | strongly disagree| disagree|  neutral|    agree| strongly agree|     total|
      |:-----------------------------------------------------------|-----------------:|--------:|--------:|--------:|--------------:|---------:|
      |ChatGPT has clear advantages compared to similar offerings. |            6% (6)|   8% (8)| 35% (34)| 36% (35)|       14% (14)| 100% (97)|
      |Using ChatGPT brings financial benefits.                    |          22% (21)| 22% (21)| 30% (29)| 21% (20)|         6% (6)| 100% (97)|
      |Using ChatGPT is advantageous in many tasks.                |            6% (6)| 10% (10)| 21% (20)| 46% (45)|       16% (16)| 100% (97)|
      |Compared to other systems, using ChatGPT is more fun.       |            6% (6)|   4% (4)| 36% (35)| 39% (38)|       14% (14)| 100% (97)|
      |Much can go wrong when using ChatGPT.                       |            6% (6)| 27% (26)| 32% (31)| 22% (21)|       13% (13)| 100% (97)|
      |There are legal issues with using ChatGPT.                  |          10% (10)| 19% (18)| 41% (40)| 14% (14)|       15% (15)| 100% (97)|
      |The security of user data is not guaranteed with ChatGPT.   |            3% (3)| 22% (21)| 42% (41)| 19% (18)|       14% (14)| 100% (97)|
      |Using ChatGPT could bring personal disadvantages.           |          11% (11)| 35% (34)| 29% (28)| 18% (17)|         7% (7)| 100% (97)|
      |In my environment, using ChatGPT is standard.               |          20% (19)| 34% (33)| 26% (25)| 15% (15)|         5% (5)| 100% (97)|
      |Almost everyone in my environment uses ChatGPT.             |          27% (26)| 31% (30)| 26% (25)| 10% (10)|         6% (6)| 100% (97)|
      |Not using ChatGPT is considered being an outsider.          |          46% (45)| 27% (26)| 14% (14)|   7% (7)|         5% (5)| 100% (97)|
      |Using ChatGPT brings me recognition from my environment.    |          33% (32)| 27% (26)| 21% (20)| 13% (13)|         6% (6)| 100% (97)|
      
      5 missing case(s) ommited.
      

# Distribution table for age

    Code
      volker::tab_metrics(data, sd_age)
    Output
      
      
      |Age    | value|
      |:------|-----:|
      |min    |    18|
      |q1     |    27|
      |median |    38|
      |q3     |    52|
      |max    |    68|
      |mean   |  39.7|
      |sd     |  13.8|
      |n      |   101|

# Group comparison of a metric variable

    Code
      volker::tab_metrics(data, sd_age, sd_gender)
    Output
      
      
      |Gender  | min|   q1| median|   q3| max| mean|   sd|   n|
      |:-------|---:|----:|------:|----:|---:|----:|----:|---:|
      |female  |  18| 25.8|   38.0| 44.2|  63| 37.5| 13.4|  40|
      |male    |  19| 32.5|   38.5| 52.0|  68| 41.2| 14.0|  60|
      |diverse |  33| 33.0|   33.0| 33.0|  33| 33.0|     |   1|
      |total   |  18| 27.0|   38.0| 52.0|  68| 39.7| 13.8| 101|

# Compare means of multiple items

    Code
      volker::tab_metrics(data, use_work, use_private, metric = TRUE)
    Output
      
      
      |Item 1                  |             Item 2|   n| Pearson's r|
      |:-----------------------|------------------:|---:|-----------:|
      |in professional context | in private context| 101|        0.37|

---

    Code
      volker::tab_metrics(data, tidyselect::starts_with("cg_adoption_"), sd_gender)
    Output
      
      
      |Expectations                                                |     total|    female|      male|  diverse|
      |:-----------------------------------------------------------|---------:|---------:|---------:|--------:|
      |ChatGPT has clear advantages compared to similar offerings. | 3.4 (1.0)| 3.6 (1.0)| 3.3 (1.0)| 4.0 (NA)|
      |Using ChatGPT brings financial benefits.                    | 2.7 (1.2)| 2.6 (1.2)| 2.7 (1.2)| 3.0 (NA)|
      |Using ChatGPT is advantageous in many tasks.                | 3.6 (1.1)| 3.7 (1.0)| 3.5 (1.1)| 4.0 (NA)|
      |Compared to other systems, using ChatGPT is more fun.       | 3.5 (1.0)| 3.6 (1.0)| 3.5 (1.0)| 3.0 (NA)|
      |Much can go wrong when using ChatGPT.                       | 3.1 (1.1)| 3.1 (1.0)| 3.1 (1.2)| 3.0 (NA)|
      |There are legal issues with using ChatGPT.                  | 3.1 (1.2)| 3.0 (1.0)| 3.1 (1.3)| 3.0 (NA)|
      |The security of user data is not guaranteed with ChatGPT.   | 3.2 (1.0)| 3.0 (1.0)| 3.3 (1.1)| 3.0 (NA)|
      |Using ChatGPT could bring personal disadvantages.           | 2.7 (1.1)| 2.5 (0.9)| 2.8 (1.2)| 4.0 (NA)|
      |In my environment, using ChatGPT is standard.               | 2.5 (1.1)| 2.5 (0.9)| 2.5 (1.3)| 4.0 (NA)|
      |Almost everyone in my environment uses ChatGPT.             | 2.4 (1.2)| 2.4 (1.0)| 2.3 (1.3)| 4.0 (NA)|
      |Not using ChatGPT is considered being an outsider.          | 2.0 (1.2)| 1.8 (1.0)| 2.1 (1.3)| 4.0 (NA)|
      |Using ChatGPT brings me recognition from my environment.    | 2.3 (1.2)| 2.4 (1.2)| 2.3 (1.3)| 3.0 (NA)|
      
      5 missing case(s) ommited.
      

---

    Code
      volker::tab_metrics(data, tidyselect::starts_with("cg_adoption_"), sd_age,
      metric = TRUE)
    Output
      
      
      |Expectations                                                |   Age|
      |:-----------------------------------------------------------|-----:|
      |ChatGPT has clear advantages compared to similar offerings. | -0.12|
      |Using ChatGPT brings financial benefits.                    | -0.13|
      |Using ChatGPT is advantageous in many tasks.                | -0.09|
      |Compared to other systems, using ChatGPT is more fun.       | -0.12|
      |Much can go wrong when using ChatGPT.                       |  0.18|
      |There are legal issues with using ChatGPT.                  |  0.24|
      |The security of user data is not guaranteed with ChatGPT.   | -0.10|
      |Using ChatGPT could bring personal disadvantages.           |  0.02|
      |In my environment, using ChatGPT is standard.               | -0.17|
      |Almost everyone in my environment uses ChatGPT.             | -0.06|
      |Not using ChatGPT is considered being an outsider.          |  0.09|
      |Using ChatGPT brings me recognition from my environment.    | -0.02|
      
      5 missing case(s) ommited.
      

---

    Code
      volker::tab_metrics(data, tidyselect::starts_with("cg_adoption_"), tidyselect::starts_with(
        "use_"), metric = TRUE)
    Output
      
      
      |Expectations                                                | Usage: in private context| Usage: in professional context|
      |:-----------------------------------------------------------|-------------------------:|------------------------------:|
      |ChatGPT has clear advantages compared to similar offerings. |                      0.50|                           0.28|
      |Using ChatGPT brings financial benefits.                    |                      0.21|                           0.54|
      |Using ChatGPT is advantageous in many tasks.                |                      0.37|                           0.37|
      |Compared to other systems, using ChatGPT is more fun.       |                      0.47|                           0.29|
      |Much can go wrong when using ChatGPT.                       |                     -0.24|                          -0.09|
      |There are legal issues with using ChatGPT.                  |                      0.08|                           0.31|
      |The security of user data is not guaranteed with ChatGPT.   |                      0.02|                           0.20|
      |Using ChatGPT could bring personal disadvantages.           |                     -0.09|                           0.15|
      |In my environment, using ChatGPT is standard.               |                      0.40|                           0.58|
      |Almost everyone in my environment uses ChatGPT.             |                      0.47|                           0.55|
      |Not using ChatGPT is considered being an outsider.          |                      0.34|                           0.34|
      |Using ChatGPT brings me recognition from my environment.    |                      0.42|                           0.46|
      
      5 missing case(s) ommited.
      

# Distribution table for multiple metric items

    Code
      volker::tab_metrics(data, tidyselect::starts_with("cg_adoption_"))
    Output
      
      
      |Expectations                                                | min| q1| median| q3| max| mean|  sd|  n|
      |:-----------------------------------------------------------|---:|--:|------:|--:|---:|----:|---:|--:|
      |ChatGPT has clear advantages compared to similar offerings. |   1|  3|      4|  4|   5|  3.4| 1.0| 97|
      |Using ChatGPT brings financial benefits.                    |   1|  2|      3|  4|   5|  2.7| 1.2| 97|
      |Using ChatGPT is advantageous in many tasks.                |   1|  3|      4|  4|   5|  3.6| 1.1| 97|
      |Compared to other systems, using ChatGPT is more fun.       |   1|  3|      4|  4|   5|  3.5| 1.0| 97|
      |Much can go wrong when using ChatGPT.                       |   1|  2|      3|  4|   5|  3.1| 1.1| 97|
      |There are legal issues with using ChatGPT.                  |   1|  2|      3|  4|   5|  3.1| 1.2| 97|
      |The security of user data is not guaranteed with ChatGPT.   |   1|  3|      3|  4|   5|  3.2| 1.0| 97|
      |Using ChatGPT could bring personal disadvantages.           |   1|  2|      3|  3|   5|  2.7| 1.1| 97|
      |In my environment, using ChatGPT is standard.               |   1|  2|      2|  3|   5|  2.5| 1.1| 97|
      |Almost everyone in my environment uses ChatGPT.             |   1|  1|      2|  3|   5|  2.4| 1.2| 97|
      |Not using ChatGPT is considered being an outsider.          |   1|  1|      2|  3|   5|  2.0| 1.2| 97|
      |Using ChatGPT brings me recognition from my environment.    |   1|  1|      2|  3|   5|  2.3| 1.2| 97|
      
      5 missing case(s) ommited.
      

# missing values make no trouble

    Code
      .
    Output
      
      
      |cg_adoption  |     total|  diverse|    female|      male|
      |:------------|---------:|--------:|---------:|---------:|
      |advantage_01 | 3.4 (1.0)| 4.0 (NA)| 3.6 (1.0)| 3.3 (1.0)|
      |advantage_02 | 2.7 (1.2)| 3.0 (NA)| 2.6 (1.2)| 2.7 (1.2)|
      |advantage_03 | 3.6 (1.1)| 4.0 (NA)| 3.7 (1.0)| 3.5 (1.1)|
      |advantage_04 | 3.5 (1.0)| 3.0 (NA)| 3.6 (1.0)| 3.5 (1.0)|
      |fearofuse_01 | 3.1 (1.1)| 3.0 (NA)| 3.1 (1.0)| 3.1 (1.2)|
      |fearofuse_02 | 3.1 (1.2)| 3.0 (NA)| 3.0 (1.0)| 3.1 (1.3)|
      |fearofuse_03 | 3.2 (1.0)| 3.0 (NA)| 3.0 (1.0)| 3.3 (1.1)|
      |fearofuse_04 | 2.7 (1.1)| 4.0 (NA)| 2.5 (0.9)| 2.8 (1.2)|
      |social_01    | 2.5 (1.1)| 4.0 (NA)| 2.5 (0.9)| 2.5 (1.3)|
      |social_02    | 2.4 (1.2)| 4.0 (NA)| 2.4 (1.0)| 2.3 (1.3)|
      |social_03    | 2.0 (1.2)| 4.0 (NA)| 1.8 (1.0)| 2.1 (1.3)|
      |social_04    | 2.3 (1.2)| 3.0 (NA)| 2.4 (1.2)| 2.3 (1.3)|
      
      41 missing case(s) ommited.
      

# Correlation of items

    Code
      volker::tab_metrics_items_cor(data, tidyselect::starts_with("cg_adoption_"),
      tidyselect::starts_with("cg_adoption_"))
    Output
      
      
      |Expectations                                                | ChatGPT has clear advantages compared to similar offerings.| Using ChatGPT brings financial benefits.| Using ChatGPT is advantageous in many tasks.| Compared to other systems, using ChatGPT is more fun.| Much can go wrong when using ChatGPT.| There are legal issues with using ChatGPT.| The security of user data is not guaranteed with ChatGPT.| Using ChatGPT could bring personal disadvantages.| In my environment, using ChatGPT is standard.| Almost everyone in my environment uses ChatGPT.| Not using ChatGPT is considered being an outsider.| Using ChatGPT brings me recognition from my environment.|
      |:-----------------------------------------------------------|-----------------------------------------------------------:|----------------------------------------:|--------------------------------------------:|-----------------------------------------------------:|-------------------------------------:|------------------------------------------:|---------------------------------------------------------:|-------------------------------------------------:|---------------------------------------------:|-----------------------------------------------:|--------------------------------------------------:|--------------------------------------------------------:|
      |ChatGPT has clear advantages compared to similar offerings. |                                                        1.00|                                     0.37|                                         0.64|                                                  0.61|                                 -0.14|                                       0.19|                                                      0.07|                                              0.01|                                          0.21|                                            0.28|                                               0.16|                                                     0.27|
      |Using ChatGPT brings financial benefits.                    |                                                        0.37|                                     1.00|                                         0.46|                                                  0.42|                                  0.02|                                       0.34|                                                      0.34|                                              0.22|                                          0.54|                                            0.50|                                               0.36|                                                     0.40|
      |Using ChatGPT is advantageous in many tasks.                |                                                        0.64|                                     0.46|                                         1.00|                                                  0.47|                                 -0.11|                                       0.19|                                                      0.06|                                              0.09|                                          0.33|                                            0.34|                                               0.13|                                                     0.31|
      |Compared to other systems, using ChatGPT is more fun.       |                                                        0.61|                                     0.42|                                         0.47|                                                  1.00|                                 -0.19|                                       0.17|                                                      0.14|                                             -0.07|                                          0.33|                                            0.26|                                               0.20|                                                     0.36|
      |Much can go wrong when using ChatGPT.                       |                                                       -0.14|                                     0.02|                                        -0.11|                                                 -0.19|                                  1.00|                                       0.29|                                                      0.36|                                              0.48|                                         -0.19|                                            0.04|                                               0.10|                                                    -0.07|
      |There are legal issues with using ChatGPT.                  |                                                        0.19|                                     0.34|                                         0.19|                                                  0.17|                                  0.29|                                       1.00|                                                      0.38|                                              0.28|                                          0.23|                                            0.22|                                               0.33|                                                     0.30|
      |The security of user data is not guaranteed with ChatGPT.   |                                                        0.07|                                     0.34|                                         0.06|                                                  0.14|                                  0.36|                                       0.38|                                                      1.00|                                              0.39|                                          0.06|                                            0.20|                                               0.29|                                                     0.10|
      |Using ChatGPT could bring personal disadvantages.           |                                                        0.01|                                     0.22|                                         0.09|                                                 -0.07|                                  0.48|                                       0.28|                                                      0.39|                                              1.00|                                          0.25|                                            0.22|                                               0.25|                                                     0.14|
      |In my environment, using ChatGPT is standard.               |                                                        0.21|                                     0.54|                                         0.33|                                                  0.33|                                 -0.19|                                       0.23|                                                      0.06|                                              0.25|                                          1.00|                                            0.73|                                               0.48|                                                     0.57|
      |Almost everyone in my environment uses ChatGPT.             |                                                        0.28|                                     0.50|                                         0.34|                                                  0.26|                                  0.04|                                       0.22|                                                      0.20|                                              0.22|                                          0.73|                                            1.00|                                               0.58|                                                     0.54|
      |Not using ChatGPT is considered being an outsider.          |                                                        0.16|                                     0.36|                                         0.13|                                                  0.20|                                  0.10|                                       0.33|                                                      0.29|                                              0.25|                                          0.48|                                            0.58|                                               1.00|                                                     0.56|
      |Using ChatGPT brings me recognition from my environment.    |                                                        0.27|                                     0.40|                                         0.31|                                                  0.36|                                 -0.07|                                       0.30|                                                      0.10|                                              0.14|                                          0.57|                                            0.54|                                               0.56|                                                     1.00|
      
      5 missing case(s) ommited.
      

# Item order is kept

    Code
      tab_counts_items(tibble::tribble(~f1, ~f2, ~f10, 1, 1, 1, 1, 2, 1, 2, 2, 2), c(
        f1:f10))
    Output
      
      
      |f  |       1|       2|    total|
      |:--|-------:|-------:|--------:|
      |1  | 67% (2)| 33% (1)| 100% (3)|
      |2  | 33% (1)| 67% (2)| 100% (3)|
      |10 | 67% (2)| 33% (1)| 100% (3)|

