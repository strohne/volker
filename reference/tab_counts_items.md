# Output frequencies for multiple variables

Output frequencies for multiple variables

## Usage

``` r
tab_counts_items(
  data,
  cols,
  ci = FALSE,
  percent = TRUE,
  values = c("n", "p"),
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble containing item measures.

- cols:

  Tidyselect item variables (e.g. starts_with...).

- ci:

  Whether to compute 95% confidence intervals.

- percent:

  Set to FALSE to prevent calculating percents from proportions.

- values:

  The values to output: n (frequency) or p (percentage) or both (the
  default).

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [tab_counts](https://strohne.github.io/volker/reference/tab_counts.md).

## Value

A volker tibble.

## Examples

``` r
library(volker)
data <- volker::chatgpt

tab_counts_items(data, starts_with("cg_adoption_"))
#> 
#> 
#> |Expectations                             | strongly disagree| disagree|  neutral|    agree| strongly agree|     total|
#> |:----------------------------------------|-----------------:|--------:|--------:|--------:|--------------:|---------:|
#> |ChatGPT has clear advantages compared... |            6% (6)|   8% (8)| 35% (34)| 36% (35)|       14% (14)| 100% (97)|
#> |Using ChatGPT brings financial benefits. |          22% (21)| 22% (21)| 30% (29)| 21% (20)|         6% (6)| 100% (97)|
#> |Using ChatGPT is advantageous in many... |            6% (6)| 10% (10)| 21% (20)| 46% (45)|       16% (16)| 100% (97)|
#> |Compared to other systems, using Chat... |            6% (6)|   4% (4)| 36% (35)| 39% (38)|       14% (14)| 100% (97)|
#> |Much can go wrong when using ChatGPT.    |            6% (6)| 27% (26)| 32% (31)| 22% (21)|       13% (13)| 100% (97)|
#> |There are legal issues with using Cha... |          10% (10)| 19% (18)| 41% (40)| 14% (14)|       15% (15)| 100% (97)|
#> |The security of user data is not guar... |            3% (3)| 22% (21)| 42% (41)| 19% (18)|       14% (14)| 100% (97)|
#> |Using ChatGPT could bring personal di... |          11% (11)| 35% (34)| 29% (28)| 18% (17)|         7% (7)| 100% (97)|
#> |In my environment, using ChatGPT is s... |          20% (19)| 34% (33)| 26% (25)| 15% (15)|         5% (5)| 100% (97)|
#> |Almost everyone in my environment use... |          27% (26)| 31% (30)| 26% (25)| 10% (10)|         6% (6)| 100% (97)|
#> |Not using ChatGPT is considered being... |          46% (45)| 27% (26)| 14% (14)|   7% (7)|         5% (5)| 100% (97)|
#> |Using ChatGPT brings me recognition f... |          33% (32)| 27% (26)| 21% (20)| 13% (13)|         6% (6)| 100% (97)|
#> 
#> n=97. 4 missing case(s) omitted.
#> 
```
