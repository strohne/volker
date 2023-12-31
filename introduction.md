---
title: "Introduction"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  chunk_output_type: console
---



## 1. One variable


### Frequency table for one categorical variable: Gender

```r

tab_var_counts(data, sd_geschlecht)
```



|Geschlecht |   n|    p| valid|
|:----------|---:|----:|-----:|
|weiblich   |  46|  40%|   40%|
|männlich   |  68|  59%|   59%|
|divers     |   1|   1%|    1%|
|Total      | 115| 100%|  100%|

### Distribution table for one metric variable: Age

```r

tab_var_metrics(data, sd_alter)
```



|Alter   | value|
|:-------|-----:|
|min     |    18|
|q1      |  30.5|
|median  |    38|
|q3      |  49.5|
|max     |    68|
|m       |  39.8|
|sd      |  13.3|
|missing |     0|
|n       |   115|

## 2. Multiple items

### Frequency table for multiple categorical variables: Adoption factors

```r

tab_item_counts(data, starts_with("cg_adoption_"))
```



|Item                                                                      |        1|        2|        3|        4|        5| Fehlend|      Total|
|:-------------------------------------------------------------------------|--------:|--------:|--------:|--------:|--------:|-------:|----------:|
|ChatGPT hat deutliche Vorteile im Vergleich zu ähnlichen Angeboten.       |   6 (5%)|  10 (9%)| 40 (35%)| 40 (35%)| 17 (15%)|  2 (2%)| 115 (100%)|
|Die Nutzung von ChatGPT bringt finanzielle Vorteile mit sich.             | 24 (21%)| 24 (21%)| 33 (29%)| 25 (22%)|   9 (8%)|  0 (0%)| 115 (100%)|
|Die Nutzung von ChatGPT ist bei vielen Arbeiten von Vorteil.              |   6 (5%)| 13 (11%)| 25 (22%)| 51 (44%)| 20 (17%)|  0 (0%)| 115 (100%)|
|Im Vergleich zu anderen Systemen macht die Nutzung von ChatGPT mehr Spaß. |   6 (5%)|   5 (4%)| 43 (37%)| 44 (38%)| 17 (15%)|  0 (0%)| 115 (100%)|
|Bei der Nutzung von ChatGPT kann viel falsch gemacht werden.              |   6 (5%)| 28 (24%)| 37 (32%)| 27 (23%)| 17 (15%)|  0 (0%)| 115 (100%)|
|Es gibt rechtliche Probleme bei der Nutzung von ChatGPT.                  |  10 (9%)| 21 (18%)| 49 (43%)| 18 (16%)| 17 (15%)|  0 (0%)| 115 (100%)|
|Die Sicherheit von Nutzungsdaten ist bei ChatGPT nicht gewährleistet.     |   3 (3%)| 26 (23%)| 47 (41%)| 21 (18%)| 17 (15%)|  1 (1%)| 115 (100%)|
|Die Nutzung von ChatGPT könnte persönliche Nachteile mit sich bringen.    | 11 (10%)| 41 (36%)| 34 (30%)| 20 (17%)|   9 (8%)|  0 (0%)| 115 (100%)|
|In meinem Umfeld gehört die Nutzung von ChatGPT zum Standard.             | 22 (19%)| 40 (35%)| 28 (24%)| 17 (15%)|   7 (6%)|  1 (1%)| 115 (100%)|
|Fast alle Personen in meinem Umfeld nutzen ChatGPT.                       | 31 (27%)| 36 (31%)| 28 (24%)| 12 (10%)|   8 (7%)|  0 (0%)| 115 (100%)|
|Wer ChatGPT nicht nutzt, wird als Außenseiter angesehen.                  | 50 (43%)| 31 (27%)| 18 (16%)|   8 (7%)|   7 (6%)|  1 (1%)| 115 (100%)|
|Die Nutzung von ChatGPT bringt mir Anerkennung aus meinem Umfeld.         | 35 (30%)| 30 (26%)| 25 (22%)| 17 (15%)|   8 (7%)|  0 (0%)| 115 (100%)|

### Distribution table for multiple metric items: Adoption factors

```r

tab_item_metrics(data, starts_with("cg_adoption_"))
```



|Item                                                                      | min|  q1| median|  q3| max|   m|  sd| missing|   n|
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

## 3. Two variables (one being categorical)

### Cross table of categorical variables: Innovator type by gender


```r

tab_group_counts(data, in_adoption, sd_geschlecht)
```



|Typologie                                                      |      Total|  weiblich|  männlich|   divers|
|:--------------------------------------------------------------|----------:|---------:|---------:|--------:|
|Ich probiere neue Angebote sofort aus.                         |   21 (18%)|   5 (11%)|  15 (22%)| 1 (100%)|
|Ich probiere neue Angebote eher schnell aus.                   |   66 (57%)|  26 (57%)|  40 (59%)|   0 (0%)|
|Ich warte eher ab, bis sich die Angebote durchgesetzt haben.   |   26 (23%)|  15 (33%)|  11 (16%)|   0 (0%)|
|Ich nutze neue Angebote erst, wenn ich nicht mehr anders kann. |     2 (2%)|    0 (0%)|    2 (3%)|   0 (0%)|
|Total                                                          | 115 (100%)| 46 (100%)| 68 (100%)| 1 (100%)|

### Group comparison of a metric variable: Age by gender


```r

tab_group_metrics(data, sd_alter, sd_geschlecht)
```



|Geschlecht | min|   q1| median|   q3| max|    m|   sd| missing|   n|
|:----------|---:|----:|------:|----:|---:|----:|----:|-------:|---:|
|weiblich   |  18| 26.2|   37.5| 44.8|  67| 38.0| 13.4|       0|  46|
|männlich   |  19| 33.8|   38.0| 50.5|  68| 41.1| 13.3|       0|  68|
|divers     |  33| 33.0|   33.0| 33.0|  33| 33.0|   NA|       0|   1|
|Total      |  18| 30.5|   38.0| 49.5|  68| 39.8| 13.3|       0| 115|

## 4. Multiple metric variables

### Compare means of multiple items: Adoption factors by gender

```r

tab_multi_means(data, starts_with("cg_adoption_"), sd_geschlecht)
```



|Item                                                                      |     Total|  weiblich|  männlich|   divers|
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

### Correlation of items: Adoption factors by adoption factors

```r

tab_multi_corr(data, starts_with("cg_adoption_"))
```



|Item         | advantage_01| advantage_02| advantage_03| advantage_04| fearofuse_01| fearofuse_02| fearofuse_03| fearofuse_04| social_01| social_02| social_03| social_04|
|:------------|------------:|------------:|------------:|------------:|------------:|------------:|------------:|------------:|---------:|---------:|---------:|---------:|
|advantage_01 |          1.0|          0.4|          0.6|          0.6|          0.0|          0.3|          0.2|          0.1|       0.2|       0.3|       0.2|       0.3|
|advantage_02 |          0.4|          1.0|          0.5|          0.4|          0.1|          0.3|          0.4|          0.3|       0.6|       0.5|       0.4|       0.5|
|advantage_03 |          0.6|          0.5|          1.0|          0.5|          0.0|          0.2|          0.1|          0.1|       0.4|       0.3|       0.2|       0.3|
|advantage_04 |          0.6|          0.4|          0.5|          1.0|         -0.1|          0.2|          0.2|          0.0|       0.3|       0.3|       0.3|       0.4|
|fearofuse_01 |          0.0|          0.1|          0.0|         -0.1|          1.0|          0.3|          0.4|          0.5|      -0.1|       0.1|       0.2|       0.0|
|fearofuse_02 |          0.3|          0.3|          0.2|          0.2|          0.3|          1.0|          0.4|          0.4|       0.2|       0.3|       0.4|       0.3|
|fearofuse_03 |          0.2|          0.4|          0.1|          0.2|          0.4|          0.4|          1.0|          0.4|       0.1|       0.3|       0.3|       0.2|
|fearofuse_04 |          0.1|          0.3|          0.1|          0.0|          0.5|          0.4|          0.4|          1.0|       0.3|       0.3|       0.3|       0.2|
|social_01    |          0.2|          0.6|          0.4|          0.3|         -0.1|          0.2|          0.1|          0.3|       1.0|       0.7|       0.5|       0.6|
|social_02    |          0.3|          0.5|          0.3|          0.3|          0.1|          0.3|          0.3|          0.3|       0.7|       1.0|       0.6|       0.6|
|social_03    |          0.2|          0.4|          0.2|          0.3|          0.2|          0.4|          0.3|          0.3|       0.5|       0.6|       1.0|       0.6|
|social_04    |          0.3|          0.5|          0.3|          0.4|          0.0|          0.3|          0.2|          0.2|       0.6|       0.6|       0.6|       1.0|

# Set custom item labels

Item labels are stored in the comment attribute of an item variable. 
When printing item tables, the labels are used. You can set custom or 
new labels by providing a tibble with item names in the first column
and item labels in the second.


```r

newlabels <- tribble(
  ~item_name, ~item_label,
  "cg_adoption_advantage_01", "Allgemeine Vorteile",
  "cg_adoption_advantage_02", "Finanzielle Vorteile",
  "cg_adoption_advantage_03", "Vorteile bei der Arbeit",
  "cg_adoption_advantage_04", "Macht mehr Spaß"
)

data %>% 
  set_item_labels(newlabels) %>% 
  tab_item_metrics(starts_with("cg_adoption_advantage_"))
```



|Item                    | min| q1| median| q3| max|   m|  sd| missing|   n|
|:-----------------------|---:|--:|------:|--:|---:|---:|---:|-------:|---:|
|Allgemeine Vorteile     |   1|  3|      4|  4|   5| 3.5| 1.0|       2| 115|
|Finanzielle Vorteile    |   1|  2|      3|  4|   5| 2.7| 1.2|       0| 115|
|Vorteile bei der Arbeit |   1|  3|      4|  4|   5| 3.6| 1.1|       0| 115|
|Macht mehr Spaß         |   1|  3|      4|  4|   5| 3.5| 1.0|       0| 115|

