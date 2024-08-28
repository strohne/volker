# data_prepare works correctly

    Code
      .
    Output
      # A tibble: 2 x 2
         var1  var2
        <dbl> <dbl>
      1     1     4
      2     2     5

---

    Code
      .
    Output
      # A tibble: 3 x 2
         var1  var2
        <dbl> <dbl>
      1     1     4
      2     2     5
      3     5    -9

---

    Code
      .
    Output
      # A tibble: 2 x 2
         var1  var2
        <dbl> <dbl>
      1     1     4
      2     2     5

---

    Code
      .
    Output
      # A tibble: 3 x 2
         var1  var2
        <dbl> <dbl>
      1     1     4
      2     2     5
      3    -1    -3

# Residual negatives values are removed

    Code
      data_clean(tibble::tibble(var1 = c(1, 2, -1, -9)))
    Output
      # A tibble: 4 x 1
         var1
        <dbl>
      1     1
      2     2
      3    -1
      4    NA

# All negatives are removed

    Code
      data_rm_negatives(tibble::tibble(var1 = c(1, 2, -1, -9)), var1)
    Output
      # A tibble: 2 x 1
         var1
        <dbl>
      1     1
      2     2

# Negatives are kept

    Code
      data_clean(tibble::tibble(var1 = c(1, 2, -9)))
    Output
      # A tibble: 3 x 1
         var1
        <dbl>
      1     1
      2     2
      3    -9

# Baseline is extracted

    Code
      get_baseline(result)
    Output
      [1] "Frequencies based on values: agree, strongly agree. 4 missing case(s) omitted."

