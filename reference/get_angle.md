# Angle labels

Calculate angle for label adjustment based on character length.

## Usage

``` r
get_angle(
  labels,
  threshold = VLKR_PLOT_ANGLE_THRESHOLD,
  angle = VLKR_PLOT_ANGLE_VALUE
)
```

## Arguments

- labels:

  Vector of labels to check. The values are converted to characters.

- threshold:

  Length threshold beyond which the angle is applied. Default is 7
  Override with `options(vlkr.angle.threshold=10)`.

- angle:

  The angle to apply if any label exceeds the threshold. Default is 45.
  Override with `options(vlkr.angle.value=30)`.

## Value

A single angle value.
