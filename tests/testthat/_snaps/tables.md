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
      
      4 missing case(s) omitted.
      

# Frequency table for multiple categorical variables by grouping variable

    Code
      volker::tab_counts(data, tidyselect::starts_with("cg_adoption_"), sd_gender,
      category = c("agree", "strongly agree"))
    Output
      
      
      |Expectations                                                |    total|   female|     male|  diverse|
      |:-----------------------------------------------------------|--------:|--------:|--------:|--------:|
      |ChatGPT has clear advantages compared to similar offerings. | 51% (49)| 57% (21)| 46% (27)| 100% (1)|
      |Using ChatGPT brings financial benefits.                    | 27% (26)|  22% (8)| 31% (18)|   0% (0)|
      |Using ChatGPT is advantageous in many tasks.                | 63% (61)| 68% (25)| 59% (35)| 100% (1)|
      |Compared to other systems, using ChatGPT is more fun.       | 54% (52)| 59% (22)| 51% (30)|   0% (0)|
      |Much can go wrong when using ChatGPT.                       | 35% (34)| 32% (12)| 37% (22)|   0% (0)|
      |There are legal issues with using ChatGPT.                  | 30% (29)|  22% (8)| 36% (21)|   0% (0)|
      |The security of user data is not guaranteed with ChatGPT.   | 33% (32)|  24% (9)| 39% (23)|   0% (0)|
      |Using ChatGPT could bring personal disadvantages.           | 25% (24)|  16% (6)| 29% (17)| 100% (1)|
      |In my environment, using ChatGPT is standard.               | 21% (20)|  14% (5)| 24% (14)| 100% (1)|
      |Almost everyone in my environment uses ChatGPT.             | 16% (16)|  16% (6)|  15% (9)| 100% (1)|
      |Not using ChatGPT is considered being an outsider.          | 12% (12)|   3% (1)| 17% (10)| 100% (1)|
      |Using ChatGPT brings me recognition from my environment.    | 20% (19)|  16% (6)| 22% (13)|   0% (0)|
      
      Frequencies based on values: agree, strongly agree. 4 missing case(s) omitted.
      

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
      
      4 missing case(s) omitted.
      

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
      
      4 missing case(s) omitted.
      

# Correlations of an item battery with one variable

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
      
      4 missing case(s) omitted.
      

# Correlations of an item battery with one variable and spearman

    Code
      volker::tab_metrics(data, tidyselect::starts_with("cg_adoption_"), sd_age,
      metric = TRUE, method = "spearman")
    Output
      
      
      |Expectations                                                |   Age|
      |:-----------------------------------------------------------|-----:|
      |ChatGPT has clear advantages compared to similar offerings. | -0.14|
      |Using ChatGPT brings financial benefits.                    | -0.14|
      |Using ChatGPT is advantageous in many tasks.                | -0.13|
      |Compared to other systems, using ChatGPT is more fun.       | -0.14|
      |Much can go wrong when using ChatGPT.                       |  0.17|
      |There are legal issues with using ChatGPT.                  |  0.26|
      |The security of user data is not guaranteed with ChatGPT.   | -0.12|
      |Using ChatGPT could bring personal disadvantages.           |  0.01|
      |In my environment, using ChatGPT is standard.               | -0.16|
      |Almost everyone in my environment uses ChatGPT.             | -0.07|
      |Not using ChatGPT is considered being an outsider.          |  0.12|
      |Using ChatGPT brings me recognition from my environment.    | -0.01|
      
      4 missing case(s) omitted.
      

# Correlate two item batteries

    Code
      volker::tab_metrics(data, tidyselect::starts_with("cg_adoption_"), tidyselect::starts_with(
        "use_"), metric = TRUE)
    Output
      
      
      |Expectations                                                |                   Usage| Pearson's r|
      |:-----------------------------------------------------------|-----------------------:|-----------:|
      |ChatGPT has clear advantages compared to similar offerings. |      in private context|        0.50|
      |ChatGPT has clear advantages compared to similar offerings. | in professional context|        0.28|
      |Using ChatGPT brings financial benefits.                    |      in private context|        0.21|
      |Using ChatGPT brings financial benefits.                    | in professional context|        0.54|
      |Using ChatGPT is advantageous in many tasks.                |      in private context|        0.37|
      |Using ChatGPT is advantageous in many tasks.                | in professional context|        0.37|
      |Compared to other systems, using ChatGPT is more fun.       |      in private context|        0.47|
      |Compared to other systems, using ChatGPT is more fun.       | in professional context|        0.29|
      |Much can go wrong when using ChatGPT.                       |      in private context|       -0.24|
      |Much can go wrong when using ChatGPT.                       | in professional context|       -0.09|
      |There are legal issues with using ChatGPT.                  |      in private context|        0.08|
      |There are legal issues with using ChatGPT.                  | in professional context|        0.31|
      |The security of user data is not guaranteed with ChatGPT.   |      in private context|        0.02|
      |The security of user data is not guaranteed with ChatGPT.   | in professional context|        0.20|
      |Using ChatGPT could bring personal disadvantages.           |      in private context|       -0.09|
      |Using ChatGPT could bring personal disadvantages.           | in professional context|        0.15|
      |In my environment, using ChatGPT is standard.               |      in private context|        0.40|
      |In my environment, using ChatGPT is standard.               | in professional context|        0.58|
      |Almost everyone in my environment uses ChatGPT.             |      in private context|        0.47|
      |Almost everyone in my environment uses ChatGPT.             | in professional context|        0.55|
      |Not using ChatGPT is considered being an outsider.          |      in private context|        0.34|
      |Not using ChatGPT is considered being an outsider.          | in professional context|        0.34|
      |Using ChatGPT brings me recognition from my environment.    |      in private context|        0.42|
      |Using ChatGPT brings me recognition from my environment.    | in professional context|        0.46|
      
      4 missing case(s) omitted.
      

# Correlate two item batteries with ci

    Code
      volker::tab_metrics(data, starts_with("cg_adoption_adv"), starts_with("use_"),
      metric = TRUE, ci = T)
    Output
      
      
      |Expectations                                                |                   Usage| Pearson's r| ci low| ci high|
      |:-----------------------------------------------------------|-----------------------:|-----------:|------:|-------:|
      |ChatGPT has clear advantages compared to similar offerings. |      in private context|        0.50|   0.33|    0.63|
      |ChatGPT has clear advantages compared to similar offerings. | in professional context|        0.27|   0.07|    0.44|
      |Using ChatGPT brings financial benefits.                    |      in private context|        0.17|  -0.03|    0.36|
      |Using ChatGPT brings financial benefits.                    | in professional context|        0.53|   0.37|    0.66|
      |Using ChatGPT is advantageous in many tasks.                |      in private context|        0.34|   0.16|    0.51|
      |Using ChatGPT is advantageous in many tasks.                | in professional context|        0.35|   0.17|    0.51|
      |Compared to other systems, using ChatGPT is more fun.       |      in private context|        0.47|   0.30|    0.61|
      |Compared to other systems, using ChatGPT is more fun.       | in professional context|        0.27|   0.07|    0.44|
      
      2 missing case(s) omitted.
      

# Correlate two item batteries with spearmn

    Code
      volker::tab_metrics(data, tidyselect::starts_with("cg_adoption_"), tidyselect::starts_with(
        "use_"), metric = TRUE, method = "spearman", )
    Output
      
      
      |Expectations                                                |                   Usage| Spearman's rho|
      |:-----------------------------------------------------------|-----------------------:|--------------:|
      |ChatGPT has clear advantages compared to similar offerings. |      in private context|           0.47|
      |ChatGPT has clear advantages compared to similar offerings. | in professional context|           0.24|
      |Using ChatGPT brings financial benefits.                    |      in private context|           0.18|
      |Using ChatGPT brings financial benefits.                    | in professional context|           0.53|
      |Using ChatGPT is advantageous in many tasks.                |      in private context|           0.36|
      |Using ChatGPT is advantageous in many tasks.                | in professional context|           0.33|
      |Compared to other systems, using ChatGPT is more fun.       |      in private context|           0.44|
      |Compared to other systems, using ChatGPT is more fun.       | in professional context|           0.30|
      |Much can go wrong when using ChatGPT.                       |      in private context|          -0.24|
      |Much can go wrong when using ChatGPT.                       | in professional context|          -0.09|
      |There are legal issues with using ChatGPT.                  |      in private context|           0.04|
      |There are legal issues with using ChatGPT.                  | in professional context|           0.30|
      |The security of user data is not guaranteed with ChatGPT.   |      in private context|           0.01|
      |The security of user data is not guaranteed with ChatGPT.   | in professional context|           0.19|
      |Using ChatGPT could bring personal disadvantages.           |      in private context|          -0.16|
      |Using ChatGPT could bring personal disadvantages.           | in professional context|           0.13|
      |In my environment, using ChatGPT is standard.               |      in private context|           0.34|
      |In my environment, using ChatGPT is standard.               | in professional context|           0.57|
      |Almost everyone in my environment uses ChatGPT.             |      in private context|           0.42|
      |Almost everyone in my environment uses ChatGPT.             | in professional context|           0.52|
      |Not using ChatGPT is considered being an outsider.          |      in private context|           0.26|
      |Not using ChatGPT is considered being an outsider.          | in professional context|           0.30|
      |Using ChatGPT brings me recognition from my environment.    |      in private context|           0.36|
      |Using ChatGPT brings me recognition from my environment.    | in professional context|           0.40|
      
      4 missing case(s) omitted.
      

# missing values make no trouble

    Code
      .
    Output
      
      
      |cg_adoption  |     total|  diverse|    female|      male|
      |:------------|---------:|--------:|---------:|---------:|
      |advantage_01 | 3.2 (2.0)| 4.0 (NA)| 3.3 (2.2)| 3.1 (1.9)|
      |advantage_02 | 2.7 (1.2)| 3.0 (NA)| 2.6 (1.3)| 2.7 (1.2)|
      |advantage_03 | 3.6 (1.1)| 4.0 (NA)| 3.7 (1.0)| 3.5 (1.1)|
      |advantage_04 | 3.5 (1.0)| 3.0 (NA)| 3.6 (1.0)| 3.5 (1.0)|
      |fearofuse_01 | 3.1 (1.1)| 3.0 (NA)| 3.2 (1.0)| 3.1 (1.2)|
      |fearofuse_02 | 3.1 (1.2)| 3.0 (NA)| 3.0 (1.0)| 3.1 (1.3)|
      |fearofuse_03 | 3.1 (1.6)| 3.0 (NA)| 2.8 (2.1)| 3.3 (1.1)|
      |fearofuse_04 | 2.7 (1.1)| 4.0 (NA)| 2.5 (0.9)| 2.8 (1.2)|
      |social_01    | 2.4 (1.6)| 4.0 (NA)| 2.2 (2.0)| 2.5 (1.2)|
      |social_02    | 2.4 (1.2)| 4.0 (NA)| 2.3 (1.0)| 2.4 (1.2)|
      |social_03    | 1.9 (1.6)| 4.0 (NA)| 1.6 (2.0)| 2.0 (1.2)|
      |social_04    | 2.3 (1.2)| 3.0 (NA)| 2.4 (1.2)| 2.3 (1.3)|
      
      3 missing case(s) omitted.
      

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

