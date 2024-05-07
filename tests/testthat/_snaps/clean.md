# Negatives are cleaned

    Code
      data_clean(tibble::tibble(var1 = c(1, 2, -9)))
    Output
      # A tibble: 3 x 1
         var1
        <dbl>
      1     1
      2     2
      3    NA

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

