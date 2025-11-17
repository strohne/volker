# Calculate correlation between two vectors

Calculate correlation between two vectors

## Usage

``` r
get_correlation(x, y, method, category = NULL, test = TRUE)
```

## Arguments

- x:

  First vector

- y:

  Second vector

- method:

  One of "spearman" or "pearson" for metric vectors. For catecorical
  vectors, use "cramer" or "npmi".

- category:

  A vector of values to focus. Necessary for the npmi method only.

- test:

  Boolean; whether to perform significance tests.

## Value

The result of cor.test() for metric vectors, chisq.test for Cramer's V.
