# Residual negatives values are removes

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

