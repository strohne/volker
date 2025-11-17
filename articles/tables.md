# Table variations

| \#  | function                        | implemented | col scale | col count | cross scale | cross count |
|-----|---------------------------------|-------------|-----------|-----------|-------------|-------------|
| 1   | tab_counts_one                  |             | counts    | one       |             |             |
| 2   | tab_counts_one_grouped          |             | counts    | one       | counts      | one         |
| 3   | tab_counts_one_cor              |             | counts    | one       | metric      | one         |
| 4   | tab_counts_items                |             | counts    | multiple  |             |             |
| 5   | tab_counts_items_grouped        |             | counts    | multiple  | counts      | one         |
| 6   | tab_counts_items_grouped_items  |             | counts    | multiple  | counts      | multiple    |
| 7   | tab_counts_items_cor            |             | counts    | multiple  | metric      | one         |
| 8   | tab_counts_items_cor_items      | not yet     | counts    | multiple  | metric      | multiple    |
| 9   | tab_metrics_one                 |             | metric    | one       |             |             |
| 10  | tab_metrics_one_grouped         |             | metric    | one       | counts      | one         |
| 11  | tab_metrics_one_cor             |             | metric    | one       | metric      | one         |
| 12  | tab_metrics_items               |             | metric    | multiple  |             |             |
| 13  | tab_metrics_items_grouped       |             | metric    | multiple  | counts      | one         |
| 14  | tab_metrics_items_grouped_items | not yet     | metric    | multiple  | counts      | multiple    |
| 15  | tab_metrics_items_cor           |             | metric    | multiple  | metric      | one         |
| 16  | tab_metrics_items_cor_items     |             | metric    | multiple  | metric      | multiple    |

## Frequencies of categorical variables

### 1. One variable: Frequency table of gender

``` r
tab_counts(data, sd_gender)
```

| Gender  |   n |    p |
|:--------|----:|-----:|
| female  |  40 |  40% |
| male    |  60 |  59% |
| diverse |   1 |   1% |
| total   | 101 | 100% |

n=101.

### 2. Two variables: Cross table of innovator type by gender

``` r
tab_counts(data, adopter, sd_gender, prop="cols")
```

[TABLE]

n=101.

### 3. Two variables: One categorical and one metric variable split at the median

``` r
tab_counts(data, adopter, sd_age, metric=T)
```

[TABLE]

n=101. Age split at median 38.

### 4. Multiple items: Frequency table of adoption factors

``` r
tab_counts(data, starts_with("cg_adoption_"))
```

| Expectations                                                | strongly disagree | disagree |  neutral |    agree | strongly agree |     total |
|:------------------------------------------------------------|------------------:|---------:|---------:|---------:|---------------:|----------:|
| ChatGPT has clear advantages compared to similar offerings. |            6% (6) |   8% (8) | 35% (34) | 36% (35) |       14% (14) | 100% (97) |
| Using ChatGPT brings financial benefits.                    |          22% (21) | 22% (21) | 30% (29) | 21% (20) |         6% (6) | 100% (97) |
| Using ChatGPT is advantageous in many tasks.                |            6% (6) | 10% (10) | 21% (20) | 46% (45) |       16% (16) | 100% (97) |
| Compared to other systems, using ChatGPT is more fun.       |            6% (6) |   4% (4) | 36% (35) | 39% (38) |       14% (14) | 100% (97) |
| Much can go wrong when using ChatGPT.                       |            6% (6) | 27% (26) | 32% (31) | 22% (21) |       13% (13) | 100% (97) |
| There are legal issues with using ChatGPT.                  |          10% (10) | 19% (18) | 41% (40) | 14% (14) |       15% (15) | 100% (97) |
| The security of user data is not guaranteed with ChatGPT.   |            3% (3) | 22% (21) | 42% (41) | 19% (18) |       14% (14) | 100% (97) |
| Using ChatGPT could bring personal disadvantages.           |          11% (11) | 35% (34) | 29% (28) | 18% (17) |         7% (7) | 100% (97) |
| In my environment, using ChatGPT is standard.               |          20% (19) | 34% (33) | 26% (25) | 15% (15) |         5% (5) | 100% (97) |
| Almost everyone in my environment uses ChatGPT.             |          27% (26) | 31% (30) | 26% (25) | 10% (10) |         6% (6) | 100% (97) |
| Not using ChatGPT is considered being an outsider.          |          46% (45) | 27% (26) | 14% (14) |   7% (7) |         5% (5) | 100% (97) |
| Using ChatGPT brings me recognition from my environment.    |          33% (32) | 27% (26) | 21% (20) | 13% (13) |         6% (6) | 100% (97) |

n=97. 4 missing case(s) omitted.

### 5. Multiple grouped items: Frequency table of adoption factors grouped by gender

``` r
tab_counts(
  data, starts_with("cg_adoption_"), sd_gender, 
  category = c("agree", "strongly agree")
)
```

| Expectations                                                |    total |   female |     male |  diverse |
|:------------------------------------------------------------|---------:|---------:|---------:|---------:|
| ChatGPT has clear advantages compared to similar offerings. | 51% (49) | 57% (21) | 46% (27) | 100% (1) |
| Using ChatGPT brings financial benefits.                    | 27% (26) |  22% (8) | 31% (18) |   0% (0) |
| Using ChatGPT is advantageous in many tasks.                | 63% (61) | 68% (25) | 59% (35) | 100% (1) |
| Compared to other systems, using ChatGPT is more fun.       | 54% (52) | 59% (22) | 51% (30) |   0% (0) |
| Much can go wrong when using ChatGPT.                       | 35% (34) | 32% (12) | 37% (22) |   0% (0) |
| There are legal issues with using ChatGPT.                  | 30% (29) |  22% (8) | 36% (21) |   0% (0) |
| The security of user data is not guaranteed with ChatGPT.   | 33% (32) |  24% (9) | 39% (23) |   0% (0) |
| Using ChatGPT could bring personal disadvantages.           | 25% (24) |  16% (6) | 29% (17) | 100% (1) |
| In my environment, using ChatGPT is standard.               | 21% (20) |  14% (5) | 24% (14) | 100% (1) |
| Almost everyone in my environment uses ChatGPT.             | 16% (16) |  16% (6) |  15% (9) | 100% (1) |
| Not using ChatGPT is considered being an outsider.          | 12% (12) |   3% (1) | 17% (10) | 100% (1) |
| Using ChatGPT brings me recognition from my environment.    | 20% (19) |  16% (6) | 22% (13) |   0% (0) |

n=97. Frequencies based on values: agree, strongly agree. 4 missing
case(s) omitted.

### 6. Correlation of categorical items with categorical items

``` r
tab_counts(data, starts_with("cg_adoption_adv"), starts_with("use_"))
```

| Expectations                                                |                   Usage | Cramer’s V |
|:------------------------------------------------------------|------------------------:|-----------:|
| ChatGPT has clear advantages compared to similar offerings. |      in private context |       0.32 |
| ChatGPT has clear advantages compared to similar offerings. | in professional context |       0.24 |
| Using ChatGPT brings financial benefits.                    |      in private context |       0.24 |
| Using ChatGPT brings financial benefits.                    | in professional context |       0.37 |
| Using ChatGPT is advantageous in many tasks.                |      in private context |       0.25 |
| Using ChatGPT is advantageous in many tasks.                | in professional context |       0.30 |
| Compared to other systems, using ChatGPT is more fun.       |      in private context |       0.30 |
| Compared to other systems, using ChatGPT is more fun.       | in professional context |       0.20 |

n=99. 2 missing case(s) omitted.

### 7. Multiple categorical items and one metric variable

``` r

tab_counts(
  data, starts_with("cg_adoption_"), sd_age, metric = TRUE,
  category = c("agree", "strongly agree")
)
```

| Expectations                                                |    total |  Low Age | High Age |
|:------------------------------------------------------------|---------:|---------:|---------:|
| ChatGPT has clear advantages compared to similar offerings. | 51% (49) | 58% (26) | 44% (23) |
| Using ChatGPT brings financial benefits.                    | 27% (26) | 36% (16) | 19% (10) |
| Using ChatGPT is advantageous in many tasks.                | 63% (61) | 69% (31) | 58% (30) |
| Compared to other systems, using ChatGPT is more fun.       | 54% (52) | 58% (26) | 50% (26) |
| Much can go wrong when using ChatGPT.                       | 35% (34) | 33% (15) | 37% (19) |
| There are legal issues with using ChatGPT.                  | 30% (29) | 24% (11) | 35% (18) |
| The security of user data is not guaranteed with ChatGPT.   | 33% (32) | 42% (19) | 25% (13) |
| Using ChatGPT could bring personal disadvantages.           | 25% (24) | 29% (13) | 21% (11) |
| In my environment, using ChatGPT is standard.               | 21% (20) | 31% (14) |  12% (6) |
| Almost everyone in my environment uses ChatGPT.             | 16% (16) |  20% (9) |  13% (7) |
| Not using ChatGPT is considered being an outsider.          | 12% (12) |  18% (8) |   8% (4) |
| Using ChatGPT brings me recognition from my environment.    | 20% (19) | 24% (11) |  15% (8) |

n=97. Frequencies based on values: agree, strongly agree. Age split at
median 38. 4 missing case(s) omitted.

### 8. Correlation of categorical items with metric items

``` r
# TODO
#tab_counts(data, starts_with("cg_adoption_adv"), starts_with("cg_adoption_adv"), metric = TRUE)
```

## Distributions of metric variables

### 9. Distribution table for one metric variable: Age

``` r

tab_metrics(data, sd_age)
```

| Age    | value |
|:-------|------:|
| min    |    18 |
| q1     |    27 |
| median |    38 |
| q3     |    52 |
| max    |    68 |
| mean   |  39.7 |
| sd     |  13.8 |
| n      |   101 |

n=101.

### 10. Group comparison of a metric variable: Age by gender

``` r
tab_metrics(data, sd_age, sd_gender)
```

| Gender  | min |   q1 | median |   q3 | max | mean |   sd |   n |
|:--------|----:|-----:|-------:|-----:|----:|-----:|-----:|----:|
| female  |  18 | 25.8 |   38.0 | 44.2 |  63 | 37.5 | 13.4 |  40 |
| male    |  19 | 32.5 |   38.5 | 52.0 |  68 | 41.2 | 14.0 |  60 |
| diverse |  33 | 33.0 |   33.0 | 33.0 |  33 | 33.0 |      |   1 |
| total   |  18 | 27.0 |   38.0 | 52.0 |  68 | 39.7 | 13.8 | 101 |

n=101.

### 11. Correlation of two single variables

``` r
tab_metrics(data, use_private, use_work, metric=T, labels=F)
```

| Item 1  | Item 2 |   n | Pearson’s r |
|:--------|-------:|----:|------------:|
| private |   work | 101 |        0.37 |

n=101.

### 12. Distribution table for multiple metric items: Adoption factors

``` r
tab_metrics(data, starts_with("cg_adoption_"))
```

| Expectations                                                | min |  q1 | median |  q3 | max | mean |  sd |   n |
|:------------------------------------------------------------|----:|----:|-------:|----:|----:|-----:|----:|----:|
| ChatGPT has clear advantages compared to similar offerings. |   1 |   3 |      4 |   4 |   5 |  3.4 | 1.0 |  97 |
| Using ChatGPT brings financial benefits.                    |   1 |   2 |      3 |   4 |   5 |  2.7 | 1.2 |  97 |
| Using ChatGPT is advantageous in many tasks.                |   1 |   3 |      4 |   4 |   5 |  3.6 | 1.1 |  97 |
| Compared to other systems, using ChatGPT is more fun.       |   1 |   3 |      4 |   4 |   5 |  3.5 | 1.0 |  97 |
| Much can go wrong when using ChatGPT.                       |   1 |   2 |      3 |   4 |   5 |  3.1 | 1.1 |  97 |
| There are legal issues with using ChatGPT.                  |   1 |   2 |      3 |   4 |   5 |  3.1 | 1.2 |  97 |
| The security of user data is not guaranteed with ChatGPT.   |   1 |   3 |      3 |   4 |   5 |  3.2 | 1.0 |  97 |
| Using ChatGPT could bring personal disadvantages.           |   1 |   2 |      3 |   3 |   5 |  2.7 | 1.1 |  97 |
| In my environment, using ChatGPT is standard.               |   1 |   2 |      2 |   3 |   5 |  2.5 | 1.1 |  97 |
| Almost everyone in my environment uses ChatGPT.             |   1 |   1 |      2 |   3 |   5 |  2.4 | 1.2 |  97 |
| Not using ChatGPT is considered being an outsider.          |   1 |   1 |      2 |   3 |   5 |  2.0 | 1.2 |  97 |
| Using ChatGPT brings me recognition from my environment.    |   1 |   1 |      2 |   3 |   5 |  2.3 | 1.2 |  97 |

n=97. 4 missing case(s) omitted.

### 13. Compare means of multiple items: Adoption factors by gender

``` r
tab_metrics(data, starts_with("cg_adoption_"), sd_gender)
```

[TABLE]

n=97. 4 missing case(s) omitted.

### 14. Correlation of metric items with categorical items

``` r
# TODO
# tab_metrics(data, starts_with("cg_adoption_adv"), starts_with("use_"))
```

### 15. Correlation of items with one single variable

``` r
tab_metrics(data, starts_with("cg_adoption_adv"), sd_age, metric=T)
```

| Expectations                                                |   Age |
|:------------------------------------------------------------|------:|
| ChatGPT has clear advantages compared to similar offerings. | -0.12 |
| Using ChatGPT brings financial benefits.                    | -0.09 |
| Using ChatGPT is advantageous in many tasks.                | -0.06 |
| Compared to other systems, using ChatGPT is more fun.       | -0.12 |

n=99. 2 missing case(s) omitted.

### 16. Correlation of metric items with metric items

``` r

tab_metrics(
  data, 
  starts_with("cg_adoption_adv"), 
  starts_with("use_"),
  metric = TRUE, ci = T
)
```

| Expectations                                                |                   Usage | Pearson’s r | ci low | ci high |
|:------------------------------------------------------------|------------------------:|------------:|-------:|--------:|
| ChatGPT has clear advantages compared to similar offerings. |      in private context |        0.50 |   0.33 |    0.63 |
| ChatGPT has clear advantages compared to similar offerings. | in professional context |        0.27 |   0.07 |    0.44 |
| Using ChatGPT brings financial benefits.                    |      in private context |        0.17 |  -0.03 |    0.36 |
| Using ChatGPT brings financial benefits.                    | in professional context |        0.53 |   0.37 |    0.66 |
| Using ChatGPT is advantageous in many tasks.                |      in private context |        0.34 |   0.16 |    0.51 |
| Using ChatGPT is advantageous in many tasks.                | in professional context |        0.35 |   0.17 |    0.51 |
| Compared to other systems, using ChatGPT is more fun.       |      in private context |        0.47 |   0.30 |    0.61 |
| Compared to other systems, using ChatGPT is more fun.       | in professional context |        0.27 |   0.07 |    0.44 |

n=99. 2 missing case(s) omitted. Adjusted significance p values with fdr
method.
