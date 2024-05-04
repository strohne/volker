# Frequency table

    Code
      volker::tab_counts_one(data, sd_gender)
    Output
      
      
      |Gender  |   n|    p|
      |:-------|---:|----:|
      |female  |  40|  40%|
      |male    |  60|  59%|
      |diverse |   1|   1%|
      |Total   | 101| 100%|
      |Missing |   0|     |

# Distribution table for age

    Code
      volker::tab_metrics_one(data, sd_age)
    Output
      
      
      |Age     | value|
      |:-------|-----:|
      |min     |    18|
      |q1      |    27|
      |median  |    38|
      |q3      |    52|
      |max     |    68|
      |mean    |  39.7|
      |sd      |  13.8|
      |missing |     0|
      |n       |   101|

# Frequency table for multiple categorical variables

    Code
      volker::tab_counts_items(data, tidyselect::starts_with("cg_adoption_"),
      missings = TRUE)
    Output
      
      
      |Expectations                                                | strongly disagree| disagree|  neutral|    agree| strongly agree|     Total| Missing|
      |:-----------------------------------------------------------|-----------------:|--------:|--------:|--------:|--------------:|---------:|-------:|
      |ChatGPT has clear advantages compared to similar offerings. |            6% (6)|   8% (8)| 35% (34)| 36% (35)|       14% (14)| 100% (97)|     (2)|
      |Using ChatGPT brings financial benefits.                    |          22% (21)| 22% (21)| 30% (29)| 21% (20)|         6% (6)| 100% (97)|     (0)|
      |Using ChatGPT is advantageous in many tasks.                |            6% (6)| 10% (10)| 21% (20)| 46% (45)|       16% (16)| 100% (97)|     (0)|
      |Compared to other systems, using ChatGPT is more fun.       |            6% (6)|   4% (4)| 36% (35)| 39% (38)|       14% (14)| 100% (97)|     (0)|
      |Much can go wrong when using ChatGPT.                       |            6% (6)| 27% (26)| 32% (31)| 22% (21)|       13% (13)| 100% (97)|     (0)|
      |There are legal issues with using ChatGPT.                  |          10% (10)| 19% (18)| 41% (40)| 14% (14)|       15% (15)| 100% (97)|     (0)|
      |The security of user data is not guaranteed with ChatGPT.   |            3% (3)| 22% (21)| 42% (41)| 19% (18)|       14% (14)| 100% (97)|     (1)|
      |Using ChatGPT could bring personal disadvantages.           |          11% (11)| 35% (34)| 29% (28)| 18% (17)|         7% (7)| 100% (97)|     (0)|
      |In my environment, using ChatGPT is standard.               |          20% (19)| 34% (33)| 26% (25)| 15% (15)|         5% (5)| 100% (97)|     (1)|
      |Almost everyone in my environment uses ChatGPT.             |          27% (26)| 31% (30)| 26% (25)| 10% (10)|         6% (6)| 100% (97)|     (0)|
      |Not using ChatGPT is considered being an outsider.          |          46% (45)| 27% (26)| 14% (14)|   7% (7)|         5% (5)| 100% (97)|     (1)|
      |Using ChatGPT brings me recognition from my environment.    |          33% (32)| 27% (26)| 21% (20)| 13% (13)|         6% (6)| 100% (97)|     (0)|

# Distribution table for multiple metric items

    Code
      volker::tab_metrics_items(data, tidyselect::starts_with("cg_adoption_"))
    Output
      
      
      |Expectations                                                | min| q1| median| q3| max| mean|  sd| missing|   n|
      |:-----------------------------------------------------------|---:|--:|------:|--:|---:|----:|---:|-------:|---:|
      |ChatGPT has clear advantages compared to similar offerings. |   1|  3|      4|  4|   5|  3.5| 1.0|       2| 101|
      |Using ChatGPT brings financial benefits.                    |   1|  2|      3|  4|   5|  2.7| 1.2|       0| 101|
      |Using ChatGPT is advantageous in many tasks.                |   1|  3|      4|  4|   5|  3.6| 1.1|       0| 101|
      |Compared to other systems, using ChatGPT is more fun.       |   1|  3|      4|  4|   5|  3.5| 1.0|       0| 101|
      |Much can go wrong when using ChatGPT.                       |   1|  2|      3|  4|   5|  3.1| 1.1|       0| 101|
      |There are legal issues with using ChatGPT.                  |   1|  2|      3|  4|   5|  3.1| 1.2|       0| 101|
      |The security of user data is not guaranteed with ChatGPT.   |   1|  3|      3|  4|   5|  3.2| 1.0|       1| 101|
      |Using ChatGPT could bring personal disadvantages.           |   1|  2|      3|  3|   5|  2.7| 1.1|       0| 101|
      |In my environment, using ChatGPT is standard.               |   1|  2|      2|  3|   5|  2.5| 1.1|       1| 101|
      |Almost everyone in my environment uses ChatGPT.             |   1|  1|      2|  3|   5|  2.4| 1.2|       0| 101|
      |Not using ChatGPT is considered being an outsider.          |   1|  1|      2|  3|   5|  2.0| 1.2|       1| 101|
      |Using ChatGPT brings me recognition from my environment.    |   1|  1|      2|  3|   5|  2.3| 1.2|       0| 101|

# Cross table of categorical variables

    Code
      volker::tab_counts_one_grouped(data, adopter, sd_gender)
    Output
      
      
      |Innovator type                                    |      Total|   female|     male| diverse|
      |:-------------------------------------------------|----------:|--------:|--------:|-------:|
      |I try new offers immediately                      |   15% (15)|   2% (2)| 12% (12)|  1% (1)|
      |I try new offers rather quickly                   |   62% (63)| 25% (25)| 38% (38)|  0% (0)|
      |I wait until offers establish themselves          |   22% (22)| 13% (13)|   9% (9)|  0% (0)|
      |I only use new offers when I have no other choice |     1% (1)|   0% (0)|   1% (1)|  0% (0)|
      |Total                                             | 100% (101)| 40% (40)| 59% (60)|  1% (1)|

# Group comparison of a metric variable

    Code
      volker::tab_metrics_one_grouped(data, sd_age, sd_gender)
    Output
      
      
      |Gender  | min|   q1| median|   q3| max| mean|   sd| missing|   n|
      |:-------|---:|----:|------:|----:|---:|----:|----:|-------:|---:|
      |female  |  18| 25.8|   38.0| 44.2|  63| 37.5| 13.4|       0|  40|
      |male    |  19| 32.5|   38.5| 52.0|  68| 41.2| 14.0|       0|  60|
      |diverse |  33| 33.0|   33.0| 33.0|  33| 33.0|     |       0|   1|
      |Total   |  18| 27.0|   38.0| 52.0|  68| 39.7| 13.8|       0| 101|

# Compare means of multiple items

    Code
      volker::tab_metrics_items_grouped(data, tidyselect::starts_with("cg_adoption_"),
      sd_gender)
    Output
      
      
      |Expectations                                                |     Total|    female|      male|  diverse|
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
      

# Missing values make no trouble

    Code
      .
    Output
      
      
      |cg_adoption  |     Total|  diverse|    female|      male|
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
                  item       target value
      1   advantage_01 advantage_01     1
      2   advantage_02 advantage_01  0.37
      3   advantage_03 advantage_01  0.64
      4   advantage_04 advantage_01  0.61
      5   fearofuse_01 advantage_01 -0.14
      6   fearofuse_02 advantage_01  0.19
      7   fearofuse_03 advantage_01  0.07
      8   fearofuse_04 advantage_01  0.01
      9      social_01 advantage_01  0.21
      10     social_02 advantage_01  0.28
      11     social_03 advantage_01  0.16
      12     social_04 advantage_01  0.27
      13  advantage_01 advantage_02  0.37
      14  advantage_02 advantage_02     1
      15  advantage_03 advantage_02  0.46
      16  advantage_04 advantage_02  0.42
      17  fearofuse_01 advantage_02  0.02
      18  fearofuse_02 advantage_02  0.34
      19  fearofuse_03 advantage_02  0.34
      20  fearofuse_04 advantage_02  0.22
      21     social_01 advantage_02  0.54
      22     social_02 advantage_02   0.5
      23     social_03 advantage_02  0.36
      24     social_04 advantage_02   0.4
      25  advantage_01 advantage_03  0.64
      26  advantage_02 advantage_03  0.46
      27  advantage_03 advantage_03     1
      28  advantage_04 advantage_03  0.47
      29  fearofuse_01 advantage_03 -0.11
      30  fearofuse_02 advantage_03  0.19
      31  fearofuse_03 advantage_03  0.06
      32  fearofuse_04 advantage_03  0.09
      33     social_01 advantage_03  0.33
      34     social_02 advantage_03  0.34
      35     social_03 advantage_03  0.13
      36     social_04 advantage_03  0.31
      37  advantage_01 advantage_04  0.61
      38  advantage_02 advantage_04  0.42
      39  advantage_03 advantage_04  0.47
      40  advantage_04 advantage_04     1
      41  fearofuse_01 advantage_04 -0.19
      42  fearofuse_02 advantage_04  0.17
      43  fearofuse_03 advantage_04  0.14
      44  fearofuse_04 advantage_04 -0.07
      45     social_01 advantage_04  0.33
      46     social_02 advantage_04  0.26
      47     social_03 advantage_04   0.2
      48     social_04 advantage_04  0.36
      49  advantage_01 fearofuse_01 -0.14
      50  advantage_02 fearofuse_01  0.02
      51  advantage_03 fearofuse_01 -0.11
      52  advantage_04 fearofuse_01 -0.19
      53  fearofuse_01 fearofuse_01     1
      54  fearofuse_02 fearofuse_01  0.29
      55  fearofuse_03 fearofuse_01  0.36
      56  fearofuse_04 fearofuse_01  0.48
      57     social_01 fearofuse_01 -0.19
      58     social_02 fearofuse_01  0.04
      59     social_03 fearofuse_01   0.1
      60     social_04 fearofuse_01 -0.07
      61  advantage_01 fearofuse_02  0.19
      62  advantage_02 fearofuse_02  0.34
      63  advantage_03 fearofuse_02  0.19
      64  advantage_04 fearofuse_02  0.17
      65  fearofuse_01 fearofuse_02  0.29
      66  fearofuse_02 fearofuse_02     1
      67  fearofuse_03 fearofuse_02  0.38
      68  fearofuse_04 fearofuse_02  0.28
      69     social_01 fearofuse_02  0.23
      70     social_02 fearofuse_02  0.22
      71     social_03 fearofuse_02  0.33
      72     social_04 fearofuse_02   0.3
      73  advantage_01 fearofuse_03  0.07
      74  advantage_02 fearofuse_03  0.34
      75  advantage_03 fearofuse_03  0.06
      76  advantage_04 fearofuse_03  0.14
      77  fearofuse_01 fearofuse_03  0.36
      78  fearofuse_02 fearofuse_03  0.38
      79  fearofuse_03 fearofuse_03     1
      80  fearofuse_04 fearofuse_03  0.39
      81     social_01 fearofuse_03  0.06
      82     social_02 fearofuse_03   0.2
      83     social_03 fearofuse_03  0.29
      84     social_04 fearofuse_03   0.1
      85  advantage_01 fearofuse_04  0.01
      86  advantage_02 fearofuse_04  0.22
      87  advantage_03 fearofuse_04  0.09
      88  advantage_04 fearofuse_04 -0.07
      89  fearofuse_01 fearofuse_04  0.48
      90  fearofuse_02 fearofuse_04  0.28
      91  fearofuse_03 fearofuse_04  0.39
      92  fearofuse_04 fearofuse_04     1
      93     social_01 fearofuse_04  0.25
      94     social_02 fearofuse_04  0.22
      95     social_03 fearofuse_04  0.25
      96     social_04 fearofuse_04  0.14
      97  advantage_01    social_01  0.21
      98  advantage_02    social_01  0.54
      99  advantage_03    social_01  0.33
      100 advantage_04    social_01  0.33
      101 fearofuse_01    social_01 -0.19
      102 fearofuse_02    social_01  0.23
      103 fearofuse_03    social_01  0.06
      104 fearofuse_04    social_01  0.25
      105    social_01    social_01     1
      106    social_02    social_01  0.73
      107    social_03    social_01  0.48
      108    social_04    social_01  0.57
      109 advantage_01    social_02  0.28
      110 advantage_02    social_02   0.5
      111 advantage_03    social_02  0.34
      112 advantage_04    social_02  0.26
      113 fearofuse_01    social_02  0.04
      114 fearofuse_02    social_02  0.22
      115 fearofuse_03    social_02   0.2
      116 fearofuse_04    social_02  0.22
      117    social_01    social_02  0.73
      118    social_02    social_02     1
      119    social_03    social_02  0.58
      120    social_04    social_02  0.54
      121 advantage_01    social_03  0.16
      122 advantage_02    social_03  0.36
      123 advantage_03    social_03  0.13
      124 advantage_04    social_03   0.2
      125 fearofuse_01    social_03   0.1
      126 fearofuse_02    social_03  0.33
      127 fearofuse_03    social_03  0.29
      128 fearofuse_04    social_03  0.25
      129    social_01    social_03  0.48
      130    social_02    social_03  0.58
      131    social_03    social_03     1
      132    social_04    social_03  0.56
      133 advantage_01    social_04  0.27
      134 advantage_02    social_04   0.4
      135 advantage_03    social_04  0.31
      136 advantage_04    social_04  0.36
      137 fearofuse_01    social_04 -0.07
      138 fearofuse_02    social_04   0.3
      139 fearofuse_03    social_04   0.1
      140 fearofuse_04    social_04  0.14
      141    social_01    social_04  0.57
      142    social_02    social_04  0.54
      143    social_03    social_04  0.56
      144    social_04    social_04     1

# Item order is kept

    Code
      tab_counts_items(tibble::tribble(~f1, ~f2, ~f10, 1, 1, 1, 1, 2, 1, 2, 2, 2), c(
        f1:f10))
    Output
      
      
      |f  |       1|       2|    Total| Missing|
      |:--|-------:|-------:|--------:|-------:|
      |1  | 67% (2)| 33% (1)| 100% (3)|     (0)|
      |2  | 33% (1)| 67% (2)| 100% (3)|     (0)|
      |10 | 67% (2)| 33% (1)| 100% (3)|     (0)|

