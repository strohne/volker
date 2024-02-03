# Frequency table

    Code
      volker::tab_var_counts(data, sd_geschlecht)
    Output
      
      
      |Geschlecht |   n|    p|
      |:----------|---:|----:|
      |weiblich   |  40|  40%|
      |männlich   |  60|  59%|
      |divers     |   1|   1%|
      |Total      | 101| 100%|
      |Missing    |   0|     |

# Distribution table for age

    Code
      volker::tab_var_metrics(data, sd_alter)
    Output
      
      
      |sd_alter | value|
      |:--------|-----:|
      |min      |    18|
      |q1       |    27|
      |median   |    38|
      |q3       |    52|
      |max      |    68|
      |m        |  39.7|
      |sd       |  13.8|
      |missing  |     0|
      |n        |   101|

# Frequency table for multiple categorical variables

    Code
      volker::tab_item_counts(data, starts_with("cg_adoption_"), missings = T)
    Output
      
      
      |ChatGPT-Erwartung                                                         | trifft gar nicht zu| trifft eher nicht zu| teils teils| trifft eher zu| trifft voll zu|     Total| Missing|
      |:-------------------------------------------------------------------------|-------------------:|--------------------:|-----------:|--------------:|--------------:|---------:|-------:|
      |ChatGPT hat deutliche Vorteile im Vergleich zu ähnlichen Angeboten.       |              6% (6)|               8% (8)|    35% (34)|       36% (35)|       14% (14)| 100% (97)|     (2)|
      |Die Nutzung von ChatGPT bringt finanzielle Vorteile mit sich.             |            22% (21)|             22% (21)|    30% (29)|       21% (20)|         6% (6)| 100% (97)|     (0)|
      |Die Nutzung von ChatGPT ist bei vielen Arbeiten von Vorteil.              |              6% (6)|             10% (10)|    21% (20)|       46% (45)|       16% (16)| 100% (97)|     (0)|
      |Im Vergleich zu anderen Systemen macht die Nutzung von ChatGPT mehr Spaß. |              6% (6)|               4% (4)|    36% (35)|       39% (38)|       14% (14)| 100% (97)|     (0)|
      |Bei der Nutzung von ChatGPT kann viel falsch gemacht werden.              |              6% (6)|             27% (26)|    32% (31)|       22% (21)|       13% (13)| 100% (97)|     (0)|
      |Es gibt rechtliche Probleme bei der Nutzung von ChatGPT.                  |            10% (10)|             19% (18)|    41% (40)|       14% (14)|       15% (15)| 100% (97)|     (0)|
      |Die Sicherheit von Nutzungsdaten ist bei ChatGPT nicht gewährleistet.     |              3% (3)|             22% (21)|    42% (41)|       19% (18)|       14% (14)| 100% (97)|     (1)|
      |Die Nutzung von ChatGPT könnte persönliche Nachteile mit sich bringen.    |            11% (11)|             35% (34)|    29% (28)|       18% (17)|         7% (7)| 100% (97)|     (0)|
      |In meinem Umfeld gehört die Nutzung von ChatGPT zum Standard.             |            20% (19)|             34% (33)|    26% (25)|       15% (15)|         5% (5)| 100% (97)|     (1)|
      |Fast alle Personen in meinem Umfeld nutzen ChatGPT.                       |            27% (26)|             31% (30)|    26% (25)|       10% (10)|         6% (6)| 100% (97)|     (0)|
      |Wer ChatGPT nicht nutzt, wird als Außenseiter angesehen.                  |            46% (45)|             27% (26)|    14% (14)|         7% (7)|         5% (5)| 100% (97)|     (1)|
      |Die Nutzung von ChatGPT bringt mir Anerkennung aus meinem Umfeld.         |            33% (32)|             27% (26)|    21% (20)|       13% (13)|         6% (6)| 100% (97)|     (0)|

# Distribution table for multiple metric items

    Code
      volker::tab_item_metrics(data, starts_with("cg_adoption_"))
    Output
      
      
      |ChatGPT-Erwartung                                                         | min| q1| median| q3| max|   m|  sd| missing|   n|
      |:-------------------------------------------------------------------------|---:|--:|------:|--:|---:|---:|---:|-------:|---:|
      |ChatGPT hat deutliche Vorteile im Vergleich zu ähnlichen Angeboten.       |   1|  3|      4|  4|   5| 3.5| 1.0|       2| 101|
      |Die Nutzung von ChatGPT bringt finanzielle Vorteile mit sich.             |   1|  2|      3|  4|   5| 2.7| 1.2|       0| 101|
      |Die Nutzung von ChatGPT ist bei vielen Arbeiten von Vorteil.              |   1|  3|      4|  4|   5| 3.6| 1.1|       0| 101|
      |Im Vergleich zu anderen Systemen macht die Nutzung von ChatGPT mehr Spaß. |   1|  3|      4|  4|   5| 3.5| 1.0|       0| 101|
      |Bei der Nutzung von ChatGPT kann viel falsch gemacht werden.              |   1|  2|      3|  4|   5| 3.1| 1.1|       0| 101|
      |Es gibt rechtliche Probleme bei der Nutzung von ChatGPT.                  |   1|  2|      3|  4|   5| 3.1| 1.2|       0| 101|
      |Die Sicherheit von Nutzungsdaten ist bei ChatGPT nicht gewährleistet.     |   1|  3|      3|  4|   5| 3.2| 1.0|       1| 101|
      |Die Nutzung von ChatGPT könnte persönliche Nachteile mit sich bringen.    |   1|  2|      3|  3|   5| 2.7| 1.1|       0| 101|
      |In meinem Umfeld gehört die Nutzung von ChatGPT zum Standard.             |   1|  2|      2|  3|   5| 2.5| 1.1|       1| 101|
      |Fast alle Personen in meinem Umfeld nutzen ChatGPT.                       |   1|  1|      2|  3|   5| 2.4| 1.2|       0| 101|
      |Wer ChatGPT nicht nutzt, wird als Außenseiter angesehen.                  |   1|  1|      2|  3|   5| 2.0| 1.2|       1| 101|
      |Die Nutzung von ChatGPT bringt mir Anerkennung aus meinem Umfeld.         |   1|  1|      2|  3|   5| 2.3| 1.2|       0| 101|

# Cross table of categorical variables

    Code
      volker::tab_group_counts(data, in_adoption, sd_geschlecht)
    Output
      
      
      |Geschlecht |      Total| Ich probiere neue Angebote sofort aus.| Ich probiere neue Angebote eher schnell aus.| Ich warte eher ab, bis sich die Angebote durchgesetzt haben.| Ich nutze neue Angebote erst, wenn ich nicht mehr anders kann.|
      |:----------|----------:|--------------------------------------:|--------------------------------------------:|------------------------------------------------------------:|--------------------------------------------------------------:|
      |weiblich   |   40% (40)|                                 2% (2)|                                     25% (25)|                                                     13% (13)|                                                         0% (0)|
      |männlich   |   59% (60)|                               12% (12)|                                     38% (38)|                                                       9% (9)|                                                         1% (1)|
      |divers     |     1% (1)|                                 1% (1)|                                       0% (0)|                                                       0% (0)|                                                         0% (0)|
      |Total      | 100% (101)|                               15% (15)|                                     62% (63)|                                                     22% (22)|                                                         1% (1)|

# Group comparison of a metric variable

    Code
      volker::tab_group_metrics(data, sd_alter, sd_geschlecht)
    Output
      
      
      |Geschlecht | min|   q1| median|   q3| max|    m|   sd| missing|   n|
      |:----------|---:|----:|------:|----:|---:|----:|----:|-------:|---:|
      |weiblich   |  18| 25.8|   38.0| 44.2|  63| 37.5| 13.4|       0|  40|
      |männlich   |  19| 32.5|   38.5| 52.0|  68| 41.2| 14.0|       0|  60|
      |divers     |  33| 33.0|   33.0| 33.0|  33| 33.0|   NA|       0|   1|
      |Total      |  18| 27.0|   38.0| 52.0|  68| 39.7| 13.8|       0| 101|

# Compare means of multiple items

    Code
      volker::tab_multi_means(data, starts_with("cg_adoption_"), sd_geschlecht)
    Output
      
      
      |ChatGPT-Erwartung                                                         |     Total|  weiblich|  männlich|   divers|
      |:-------------------------------------------------------------------------|---------:|---------:|---------:|--------:|
      |ChatGPT hat deutliche Vorteile im Vergleich zu ähnlichen Angeboten.       | 3.5 (1.0)| 3.6 (1.0)| 3.3 (1.0)| 4.0 (NA)|
      |Die Nutzung von ChatGPT bringt finanzielle Vorteile mit sich.             | 2.7 (1.2)| 2.6 (1.3)| 2.7 (1.2)| 3.0 (NA)|
      |Die Nutzung von ChatGPT ist bei vielen Arbeiten von Vorteil.              | 3.6 (1.1)| 3.7 (1.0)| 3.5 (1.1)| 4.0 (NA)|
      |Im Vergleich zu anderen Systemen macht die Nutzung von ChatGPT mehr Spaß. | 3.5 (1.0)| 3.6 (1.0)| 3.5 (1.0)| 3.0 (NA)|
      |Bei der Nutzung von ChatGPT kann viel falsch gemacht werden.              | 3.1 (1.1)| 3.2 (1.0)| 3.1 (1.2)| 3.0 (NA)|
      |Es gibt rechtliche Probleme bei der Nutzung von ChatGPT.                  | 3.1 (1.2)| 3.0 (1.0)| 3.1 (1.3)| 3.0 (NA)|
      |Die Sicherheit von Nutzungsdaten ist bei ChatGPT nicht gewährleistet.     | 3.2 (1.0)| 3.1 (1.0)| 3.3 (1.1)| 3.0 (NA)|
      |Die Nutzung von ChatGPT könnte persönliche Nachteile mit sich bringen.    | 2.7 (1.1)| 2.5 (0.9)| 2.8 (1.2)| 4.0 (NA)|
      |In meinem Umfeld gehört die Nutzung von ChatGPT zum Standard.             | 2.5 (1.1)| 2.5 (0.9)| 2.5 (1.2)| 4.0 (NA)|
      |Fast alle Personen in meinem Umfeld nutzen ChatGPT.                       | 2.4 (1.2)| 2.3 (1.0)| 2.4 (1.2)| 4.0 (NA)|
      |Wer ChatGPT nicht nutzt, wird als Außenseiter angesehen.                  | 2.0 (1.2)| 1.8 (1.0)| 2.0 (1.2)| 4.0 (NA)|
      |Die Nutzung von ChatGPT bringt mir Anerkennung aus meinem Umfeld.         | 2.3 (1.2)| 2.4 (1.2)| 2.3 (1.3)| 3.0 (NA)|

---

    Code
      .
    Output
      
      
      |cg_adoption  |     Total|        X|   divers|  männlich|  weiblich|
      |:------------|---------:|--------:|--------:|---------:|---------:|
      |advantage_01 | 3.5 (1.0)| NaN (NA)| 4.0 (NA)| 3.3 (1.0)| 3.6 (1.0)|
      |advantage_02 | 2.7 (1.2)| NaN (NA)| 3.0 (NA)| 2.7 (1.2)| 2.6 (1.3)|
      |advantage_03 | 3.6 (1.1)| NaN (NA)| 4.0 (NA)| 3.5 (1.1)| 3.7 (1.0)|
      |advantage_04 | 3.5 (1.0)| NaN (NA)| 3.0 (NA)| 3.5 (1.0)| 3.6 (1.0)|
      |fearofuse_01 | 3.1 (1.1)| NaN (NA)| 3.0 (NA)| 3.1 (1.2)| 3.2 (1.0)|
      |fearofuse_02 | 3.1 (1.2)| NaN (NA)| 3.0 (NA)| 3.1 (1.3)| 3.0 (1.0)|
      |fearofuse_03 | 3.2 (1.0)| NaN (NA)| 3.0 (NA)| 3.3 (1.1)| 3.1 (1.0)|
      |fearofuse_04 | 2.7 (1.1)| NaN (NA)| 4.0 (NA)| 2.8 (1.2)| 2.5 (0.9)|
      |social_01    | 2.5 (1.1)| NaN (NA)| 4.0 (NA)| 2.5 (1.2)| 2.5 (0.9)|
      |social_02    | 2.4 (1.2)| NaN (NA)| 4.0 (NA)| 2.4 (1.2)| 2.3 (1.0)|
      |social_03    | 2.0 (1.2)| NaN (NA)| 4.0 (NA)| 2.0 (1.2)| 1.8 (1.0)|
      |social_04    | 2.3 (1.2)| NaN (NA)| 3.0 (NA)| 2.3 (1.3)| 2.4 (1.2)|

# Correlation of items

    Code
      volker::tab_multi_corr(data, starts_with("cg_adoption_"))
    Output
      
      
      |Item         | advantage_01| advantage_02| advantage_03| advantage_04| fearofuse_01| fearofuse_02| fearofuse_03| fearofuse_04| social_01| social_02| social_03| social_04|
      |:------------|------------:|------------:|------------:|------------:|------------:|------------:|------------:|------------:|---------:|---------:|---------:|---------:|
      |advantage_01 |         1***|      0.36***|      0.63***|      0.61***|        -0.13|        0.19.|         0.08|         0.01|      0.2.|    0.26**|      0.16|     0.26*|
      |advantage_02 |      0.36***|         1***|      0.47***|       0.4***|         0.03|       0.32**|      0.34***|        0.18.|   0.54***|   0.49***|   0.37***|   0.41***|
      |advantage_03 |      0.63***|      0.47***|         1***|      0.45***|        -0.09|        0.19.|         0.07|         0.07|   0.33***|    0.32**|      0.13|    0.31**|
      |advantage_04 |      0.61***|       0.4***|      0.45***|         1***|        -0.16|        0.18.|         0.15|        -0.05|     0.3**|     0.22*|      0.2*|   0.34***|
      |fearofuse_01 |        -0.13|         0.03|        -0.09|        -0.16|         1***|       0.29**|      0.37***|      0.46***|    -0.19.|      0.01|      0.09|     -0.09|
      |fearofuse_02 |        0.19.|       0.32**|        0.19.|        0.18.|       0.29**|         1***|      0.38***|       0.28**|     0.21*|     0.21*|   0.33***|    0.29**|
      |fearofuse_03 |         0.08|      0.34***|         0.07|         0.15|      0.37***|      0.38***|         1***|      0.38***|      0.06|     0.18.|    0.28**|      0.09|
      |fearofuse_04 |         0.01|        0.18.|         0.07|        -0.05|      0.46***|       0.28**|      0.38***|         1***|     0.24*|     0.21*|     0.25*|      0.12|
      |social_01    |         0.2.|      0.54***|      0.33***|        0.3**|       -0.19.|        0.21*|         0.06|        0.24*|      1***|   0.73***|   0.47***|   0.57***|
      |social_02    |       0.26**|      0.49***|       0.32**|        0.22*|         0.01|        0.21*|        0.18.|        0.21*|   0.73***|      1***|   0.57***|   0.53***|
      |social_03    |         0.16|      0.37***|         0.13|         0.2*|         0.09|      0.33***|       0.28**|        0.25*|   0.47***|   0.57***|      1***|   0.56***|
      |social_04    |        0.26*|      0.41***|       0.31**|      0.34***|        -0.09|       0.29**|         0.09|         0.12|   0.57***|   0.53***|   0.56***|      1***|

