# Interpolate an alpha value based on case numbers

Interpolate an alpha value based on case numbers

## Usage

``` r
vlkr_alpha_interpolated(
  n,
  n_min = 20,
  n_max = 100,
  alpha_min = VLKR_POINT_ALPHA,
  alpha_max = 1
)
```

## Arguments

- n:

  Number of cases

- n_min:

  The case number where the minimum alpha value starts

- n_max:

  The case number where the maximum alpha value ends

- alpha_min:

  The minimum alpha value

- alpha_max:

  The maximum alpha value

## Value

A value between the minimum and the maximum alpha value
