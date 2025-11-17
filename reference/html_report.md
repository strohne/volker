# Volker style HTML document format

Based on the standard theme, tweaks the pill navigation to switch
between tables and plots. To use the format, in the header of your
Markdown document, set `output: volker::html_report`.

## Usage

``` r
html_report(...)
```

## Arguments

- ...:

  Additional arguments passed to html_document.

## Value

R Markdown output format.

## Examples

``` r
if (FALSE) { # \dontrun{
# Add `volker::html_report` to the output options of your Markdown document:
#
# ```
# ---
# title: "How to create reports?"
# output: volker::html_report
# ---
# ```
} # }
```
