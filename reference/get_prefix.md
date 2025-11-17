# Get the common prefix of character values

Helper function taken from the biobase package. Duplicated here instead
of loading the package to avoid overhead. See
https://github.com/Bioconductor/Biobase

## Usage

``` r
get_prefix(
  x,
  ignore.case = FALSE,
  trim = FALSE,
  delimiters = c(":", "\n"),
  minlength = 3
)
```

## Arguments

- x:

  Character vector.

- ignore.case:

  Whether case matters (default).

- trim:

  Whether non alphabetic characters should be trimmed.

- delimiters:

  A list of prefix delimiters. If any of the delimiters is present in
  the extracted prefix, the part after is removed from the prefix.
  Consider the following two items as an example:
  `c("Usage: in private context", "Usage: in work context")`. The common
  prefix would be

      "Usage: in "

  , but it makes more sense to break it after the colon.

- minlength:

  Minimum length of the prefix. Consider the following two items as an
  example: `c("coder one", "cg_act_write")`. The common prefix would be

      "c"

  , although the items have nothing in common. Requirung a minimum
  common prefix length should help in many cases.

## Value

The longest common prefix of the strings.
