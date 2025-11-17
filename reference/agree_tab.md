# Agreement for multiple items

Two types of comparing categories are provided:

## Usage

``` r
agree_tab(
  data,
  cols,
  coders,
  ids = NULL,
  category = NULL,
  method = "reliability",
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble containing item measures, coders and case IDs.

- cols:

  A tidy selection of item variables (e.g. starts_with...) with ratings.

- coders:

  The column holding coders or methods to compare.

- ids:

  The column with case IDs.

- category:

  For classification performance indicators, if no category is provided,
  macro statistics are returned (along with the number of categories in
  the output). Provide a category to get the statistics for this
  category only. If values are boolean (TRUE / FALSE) and no category is
  provided, the category is always assumed to be "TRUE".

- method:

  The output metrics, one of `reliability` or `classification`. You can
  abbreviate it, e.g. `reli` or `class`.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [report_counts](https://strohne.github.io/volker/reference/report_counts.md).

## Value

A volker tibble with one row for each item. The item name is returned in
the first column. For the reliability method, the following columns are
returned:

- **n**: Number of cases (each case id is only counted once).

- **Coders**: Number of coders.

- **Categories**: Number of categories.

- **Holsti**: Percent agreement (same as accuracy).

- **Krippendorff' Alpha**: Chance-corrected reliability score.

- **Kappa**: Depending on the number of coders either Cohen's Kappa (two
  coders) or Fleiss' Kappa (more coders).

- **Gwet's AC1**: Gwet's agreement coefficient.

For the classification method, the following columns are returned:

- **n**: Number of cases (each case id is only counted once)

- **Categories**: Number of categories

- **Accuracy**: Share of correct classifications.

- **Precision**: Share of true cases in all detected true cases.

- **Recall**: Share of true cases detected from all true cases.

- **F1**: Harmonic mean of precision and recall.

## Details

- Reliability: Compare codings of two or more raters in content
  analysis. Common reliability measures are percent agreement (also
  known as Holsti), Fleiss' or Cohen's Kappa, Krippendorff's Alpha and
  Gwets AC.

- Classification: Compare true and predicted categories from
  classification methods. Common performance metrics include accuracy,
  precision, recall and F1.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(volker)

data <- volker::chatgpt

# Prepare example data.
# First, recode "x" to TRUE/FALSE for the first coder's sample.
data_coder1 <- data |>
  mutate(across(starts_with("cg_act_"), ~ ifelse(is.na(.), FALSE, TRUE))) %>%
  mutate(coder = "coder one")

# Second, recode using a dictionary approach for the second coder's sample.
data_coder2 <- data |>
  mutate(across(starts_with("cg_act_"), ~ ifelse(is.na(.), FALSE, TRUE))) %>%
  mutate(cg_act_write = grepl("write|text|translate", tolower(cg_activities))) %>%
  mutate(coder="coder two")

data_coded <- bind_rows(
  data_coder1,
  data_coder2
)

# Reliability coefficients are strictly only appropriate for manual codings
agree_tab(data_coded, cg_act_write,  coder, case, method = "reli")
#> 
#> 
#> |item         |   n| Coders| Categories| Holsti| Krippendorff's Alpha| Kappa| Gwet's AC1|
#> |:------------|---:|------:|----------:|------:|--------------------:|-----:|----------:|
#> |cg_act_write | 101|      2|          2|    0.7|                  0.4|  0.42|       0.42|
#> 
#> 
#> |item         | agree| coder one| coder two|  n|    p|
#> |:------------|-----:|---------:|---------:|--:|----:|
#> |cg_act_write |  TRUE|     FALSE|     FALSE| 42| 0.42|
#> |cg_act_write |  TRUE|      TRUE|      TRUE| 29| 0.29|
#> |cg_act_write | FALSE|     FALSE|      TRUE|  5| 0.05|
#> |cg_act_write | FALSE|      TRUE|     FALSE| 25| 0.25|

# Better use classification performance indicators to compare the
# dictionary approach with human coding
agree_tab(data_coded, cg_act_write,  coder, case, method = "class")
#> 
#> 
#> |item         |   n| Ground truth| Categories| Accuracy| Precision| Recall|   F1|
#> |:------------|---:|------------:|----------:|--------:|---------:|------:|----:|
#> |cg_act_write | 101|    coder one|       TRUE|      0.7|      0.85|   0.54| 0.66|
#> 
#> 
#> |item         | agree| coder one| coder two|  n|    p|
#> |:------------|-----:|---------:|---------:|--:|----:|
#> |cg_act_write |  TRUE|     FALSE|     FALSE| 42| 0.42|
#> |cg_act_write |  TRUE|      TRUE|      TRUE| 29| 0.29|
#> |cg_act_write | FALSE|     FALSE|      TRUE|  5| 0.05|
#> |cg_act_write | FALSE|      TRUE|     FALSE| 25| 0.25|
```
