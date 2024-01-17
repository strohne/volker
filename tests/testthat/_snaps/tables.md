# Frequency table

    Code
      volker::tab_var_counts(data, sd_geschlecht)
    Output
      
      
      |Geschlecht |   n|    p| valid|
      |:----------|---:|----:|-----:|
      |weiblich   |  46|  40%|   40%|
      |männlich   |  68|  59%|   59%|
      |divers     |   1|   1%|    1%|
      |Total      | 115| 100%|  100%|

# Distribution table for age

    Code
      volker::tab_var_metrics(data, sd_alter)
    Output
      
      
      |sd_alter | value|
      |:--------|-----:|
      |min      |    18|
      |q1       |  30.5|
      |median   |    38|
      |q3       |  49.5|
      |max      |    68|
      |m        |  39.8|
      |sd       |  13.3|
      |missing  |     0|
      |n        |   115|

# Frequency table for multiple categorical variables

    Code
      volker::tab_item_counts(data, starts_with("cg_adoption_"))
    Output
      
      
      |ChatGPT-Erwartung                                                         | trifft gar nicht zu| trifft eher nicht zu| teils teils| trifft eher zu| trifft voll zu|      Total|
      |:-------------------------------------------------------------------------|-------------------:|--------------------:|-----------:|--------------:|--------------:|----------:|
      |ChatGPT hat deutliche Vorteile im Vergleich zu ähnlichen Angeboten.       |              5% (6)|              9% (10)|    36% (40)|       34% (38)|       15% (17)| 100% (111)|
      |Die Nutzung von ChatGPT bringt finanzielle Vorteile mit sich.             |            21% (23)|             22% (24)|    29% (32)|       21% (23)|         8% (9)| 100% (111)|
      |Die Nutzung von ChatGPT ist bei vielen Arbeiten von Vorteil.              |              5% (6)|             12% (13)|    22% (24)|       44% (49)|       17% (19)| 100% (111)|
      |Im Vergleich zu anderen Systemen macht die Nutzung von ChatGPT mehr Spaß. |              5% (6)|               5% (5)|    38% (42)|       38% (42)|       14% (16)| 100% (111)|
      |Bei der Nutzung von ChatGPT kann viel falsch gemacht werden.              |              5% (6)|             25% (28)|    32% (35)|       23% (26)|       14% (16)| 100% (111)|
      |Es gibt rechtliche Probleme bei der Nutzung von ChatGPT.                  |             9% (10)|             19% (21)|    41% (46)|       15% (17)|       15% (17)| 100% (111)|
      |Die Sicherheit von Nutzungsdaten ist bei ChatGPT nicht gewährleistet.     |              3% (3)|             23% (26)|    41% (45)|       18% (20)|       15% (17)| 100% (111)|
      |Die Nutzung von ChatGPT könnte persönliche Nachteile mit sich bringen.    |            10% (11)|             34% (38)|    31% (34)|       17% (19)|         8% (9)| 100% (111)|
      |In meinem Umfeld gehört die Nutzung von ChatGPT zum Standard.             |            19% (21)|             34% (38)|    25% (28)|       15% (17)|         6% (7)| 100% (111)|
      |Fast alle Personen in meinem Umfeld nutzen ChatGPT.                       |            26% (29)|             32% (35)|    24% (27)|       11% (12)|         7% (8)| 100% (111)|
      |Wer ChatGPT nicht nutzt, wird als Außenseiter angesehen.                  |            45% (50)|             26% (29)|    15% (17)|         7% (8)|         6% (7)| 100% (111)|
      |Die Nutzung von ChatGPT bringt mir Anerkennung aus meinem Umfeld.         |            31% (34)|             25% (28)|    23% (25)|       14% (16)|         7% (8)| 100% (111)|

# Distribution table for multiple metric items

    Code
      volker::tab_item_metrics(data, starts_with("cg_adoption_"))
    Output
      
      
      |ChatGPT-Erwartung                                                         | min|  q1| median|  q3| max|   m|  sd| missing|   n|
      |:-------------------------------------------------------------------------|---:|---:|------:|---:|---:|---:|---:|-------:|---:|
      |ChatGPT hat deutliche Vorteile im Vergleich zu ähnlichen Angeboten.       |   1| 3.0|      4| 4.0|   5| 3.5| 1.0|       2| 115|
      |Die Nutzung von ChatGPT bringt finanzielle Vorteile mit sich.             |   1| 2.0|      3| 4.0|   5| 2.7| 1.2|       0| 115|
      |Die Nutzung von ChatGPT ist bei vielen Arbeiten von Vorteil.              |   1| 3.0|      4| 4.0|   5| 3.6| 1.1|       0| 115|
      |Im Vergleich zu anderen Systemen macht die Nutzung von ChatGPT mehr Spaß. |   1| 3.0|      4| 4.0|   5| 3.5| 1.0|       0| 115|
      |Bei der Nutzung von ChatGPT kann viel falsch gemacht werden.              |   1| 2.0|      3| 4.0|   5| 3.2| 1.1|       0| 115|
      |Es gibt rechtliche Probleme bei der Nutzung von ChatGPT.                  |   1| 2.0|      3| 4.0|   5| 3.1| 1.1|       0| 115|
      |Die Sicherheit von Nutzungsdaten ist bei ChatGPT nicht gewährleistet.     |   1| 2.2|      3| 4.0|   5| 3.2| 1.0|       1| 115|
      |Die Nutzung von ChatGPT könnte persönliche Nachteile mit sich bringen.    |   1| 2.0|      3| 3.5|   5| 2.8| 1.1|       0| 115|
      |In meinem Umfeld gehört die Nutzung von ChatGPT zum Standard.             |   1| 2.0|      2| 3.0|   5| 2.5| 1.1|       1| 115|
      |Fast alle Personen in meinem Umfeld nutzen ChatGPT.                       |   1| 1.0|      2| 3.0|   5| 2.4| 1.2|       0| 115|
      |Wer ChatGPT nicht nutzt, wird als Außenseiter angesehen.                  |   1| 1.0|      2| 3.0|   5| 2.0| 1.2|       1| 115|
      |Die Nutzung von ChatGPT bringt mir Anerkennung aus meinem Umfeld.         |   1| 1.0|      2| 3.0|   5| 2.4| 1.3|       0| 115|

# Cross table of categorical variables

    Code
      volker::tab_group_counts(data, in_adoption, sd_geschlecht)
    Output
      
      
      |Geschlecht |      Total| Ich probiere neue Angebote sofort aus.| Ich probiere neue Angebote eher schnell aus.| Ich warte eher ab, bis sich die Angebote durchgesetzt haben.| Ich nutze neue Angebote erst, wenn ich nicht mehr anders kann.|
      |:----------|----------:|--------------------------------------:|--------------------------------------------:|------------------------------------------------------------:|--------------------------------------------------------------:|
      |weiblich   |   40% (46)|                                 4% (5)|                                     23% (26)|                                                     13% (15)|                                                         0% (0)|
      |männlich   |   59% (68)|                               13% (15)|                                     35% (40)|                                                     10% (11)|                                                         2% (2)|
      |divers     |     1% (1)|                                 1% (1)|                                       0% (0)|                                                       0% (0)|                                                         0% (0)|
      |Total      | 100% (115)|                               18% (21)|                                     57% (66)|                                                     23% (26)|                                                         2% (2)|

# Group comparison of a metric variable

    Code
      volker::tab_group_metrics(data, sd_alter, sd_geschlecht)
    Output
      
      
      |Geschlecht | min|   q1| median|   q3| max|    m|   sd| missing|   n|
      |:----------|---:|----:|------:|----:|---:|----:|----:|-------:|---:|
      |weiblich   |  18| 26.2|   37.5| 44.8|  67| 38.0| 13.4|       0|  46|
      |männlich   |  19| 33.8|   38.0| 50.5|  68| 41.1| 13.3|       0|  68|
      |divers     |  33| 33.0|   33.0| 33.0|  33| 33.0|   NA|       0|   1|
      |Total      |  18| 30.5|   38.0| 49.5|  68| 39.8| 13.3|       0| 115|

# Compare means of multiple items

    Code
      volker::tab_multi_means(data, starts_with("cg_adoption_"), sd_geschlecht)
    Output
      
      
      |ChatGPT-Erwartung                                                         |     Total|  weiblich|  männlich|   divers|
      |:-------------------------------------------------------------------------|---------:|---------:|---------:|--------:|
      |ChatGPT hat deutliche Vorteile im Vergleich zu ähnlichen Angeboten.       | 3.5 (1.0)| 3.6 (1.0)| 3.3 (1.0)| 4.0 (NA)|
      |Die Nutzung von ChatGPT bringt finanzielle Vorteile mit sich.             | 2.7 (1.2)| 2.7 (1.3)| 2.8 (1.2)| 3.0 (NA)|
      |Die Nutzung von ChatGPT ist bei vielen Arbeiten von Vorteil.              | 3.6 (1.1)| 3.7 (1.0)| 3.5 (1.1)| 4.0 (NA)|
      |Im Vergleich zu anderen Systemen macht die Nutzung von ChatGPT mehr Spaß. | 3.5 (1.0)| 3.7 (1.0)| 3.5 (1.0)| 3.0 (NA)|
      |Bei der Nutzung von ChatGPT kann viel falsch gemacht werden.              | 3.2 (1.1)| 3.2 (1.0)| 3.1 (1.2)| 3.0 (NA)|
      |Es gibt rechtliche Probleme bei der Nutzung von ChatGPT.                  | 3.1 (1.1)| 3.1 (1.0)| 3.1 (1.2)| 3.0 (NA)|
      |Die Sicherheit von Nutzungsdaten ist bei ChatGPT nicht gewährleistet.     | 3.2 (1.0)| 3.1 (1.0)| 3.3 (1.1)| 3.0 (NA)|
      |Die Nutzung von ChatGPT könnte persönliche Nachteile mit sich bringen.    | 2.8 (1.1)| 2.7 (0.9)| 2.8 (1.2)| 4.0 (NA)|
      |In meinem Umfeld gehört die Nutzung von ChatGPT zum Standard.             | 2.5 (1.1)| 2.5 (1.0)| 2.5 (1.2)| 4.0 (NA)|
      |Fast alle Personen in meinem Umfeld nutzen ChatGPT.                       | 2.4 (1.2)| 2.4 (1.1)| 2.3 (1.2)| 4.0 (NA)|
      |Wer ChatGPT nicht nutzt, wird als Außenseiter angesehen.                  | 2.0 (1.2)| 2.0 (1.1)| 2.1 (1.2)| 4.0 (NA)|
      |Die Nutzung von ChatGPT bringt mir Anerkennung aus meinem Umfeld.         | 2.4 (1.3)| 2.5 (1.2)| 2.4 (1.3)| 3.0 (NA)|

---

    Code
      .
    Output
      
      
      |cg_adoption  |     Total|        X|   divers|  männlich|  weiblich|
      |:------------|---------:|--------:|--------:|---------:|---------:|
      |advantage_01 | 3.5 (1.0)| NaN (NA)| 4.0 (NA)| 3.3 (1.0)| 3.6 (1.0)|
      |advantage_02 | 2.7 (1.2)| NaN (NA)| 3.0 (NA)| 2.8 (1.2)| 2.7 (1.3)|
      |advantage_03 | 3.6 (1.1)| NaN (NA)| 4.0 (NA)| 3.5 (1.1)| 3.7 (1.0)|
      |advantage_04 | 3.5 (1.0)| NaN (NA)| 3.0 (NA)| 3.5 (1.0)| 3.7 (1.0)|
      |fearofuse_01 | 3.2 (1.1)| NaN (NA)| 3.0 (NA)| 3.1 (1.2)| 3.2 (1.0)|
      |fearofuse_02 | 3.1 (1.1)| NaN (NA)| 3.0 (NA)| 3.1 (1.2)| 3.1 (1.0)|
      |fearofuse_03 | 3.2 (1.0)| NaN (NA)| 3.0 (NA)| 3.3 (1.1)| 3.1 (1.0)|
      |fearofuse_04 | 2.8 (1.1)| NaN (NA)| 4.0 (NA)| 2.8 (1.2)| 2.7 (0.9)|
      |social_01    | 2.5 (1.1)| NaN (NA)| 4.0 (NA)| 2.5 (1.2)| 2.5 (1.0)|
      |social_02    | 2.4 (1.2)| NaN (NA)| 4.0 (NA)| 2.3 (1.2)| 2.4 (1.1)|
      |social_03    | 2.0 (1.2)| NaN (NA)| 4.0 (NA)| 2.1 (1.2)| 2.0 (1.1)|
      |social_04    | 2.4 (1.3)| NaN (NA)| 3.0 (NA)| 2.4 (1.3)| 2.5 (1.2)|

# Correlation of items

    Code
      volker::tab_multi_corr(data, starts_with("cg_adoption_"))
    Output
      
      
      |Item         | advantage_01| advantage_02| advantage_03| advantage_04| fearofuse_01| fearofuse_02| fearofuse_03| fearofuse_04| social_01| social_02| social_03| social_04|
      |:------------|------------:|------------:|------------:|------------:|------------:|------------:|------------:|------------:|---------:|---------:|---------:|---------:|
      |advantage_01 |         1***|      0.39***|      0.63***|      0.63***|        -0.02|       0.26**|        0.19*|          0.1|     0.23*|   0.31***|     0.23*|    0.29**|
      |advantage_02 |      0.39***|         1***|      0.52***|      0.44***|         0.12|      0.34***|      0.37***|       0.25**|   0.56***|   0.51***|   0.45***|   0.48***|
      |advantage_03 |      0.63***|      0.52***|         1***|       0.5***|        -0.02|        0.22*|         0.12|         0.12|   0.36***|   0.34***|      0.2*|   0.35***|
      |advantage_04 |      0.63***|      0.44***|       0.5***|         1***|        -0.07|        0.22*|        0.21*|         0.03|   0.35***|    0.27**|    0.27**|   0.38***|
      |fearofuse_01 |        -0.02|         0.12|        -0.02|        -0.07|         1***|      0.33***|      0.42***|      0.49***|     -0.08|      0.09|     0.18.|      0.03|
      |fearofuse_02 |       0.26**|      0.34***|        0.22*|        0.22*|      0.33***|         1***|      0.44***|      0.35***|    0.25**|    0.27**|   0.37***|   0.32***|
      |fearofuse_03 |        0.19*|      0.37***|         0.12|        0.21*|      0.42***|      0.44***|         1***|      0.44***|      0.13|    0.27**|   0.35***|     0.16.|
      |fearofuse_04 |          0.1|       0.25**|         0.12|         0.03|      0.49***|      0.35***|      0.44***|         1***|    0.28**|    0.29**|   0.33***|     0.19*|
      |social_01    |        0.23*|      0.56***|      0.36***|      0.35***|        -0.08|       0.25**|         0.13|       0.28**|      1***|   0.74***|   0.54***|   0.61***|
      |social_02    |      0.31***|      0.51***|      0.34***|       0.27**|         0.09|       0.27**|       0.27**|       0.29**|   0.74***|      1***|   0.62***|   0.58***|
      |social_03    |        0.23*|      0.45***|         0.2*|       0.27**|        0.18.|      0.37***|      0.35***|      0.33***|   0.54***|   0.62***|      1***|   0.61***|
      |social_04    |       0.29**|      0.48***|      0.35***|      0.38***|         0.03|      0.32***|        0.16.|        0.19*|   0.61***|   0.58***|   0.61***|      1***|

