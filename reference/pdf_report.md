# Volker style PDF document format

Based on the standard theme, tweaks tex headers. To use the format, in
the header of your Markdown document, set `output: volker::pdf_report`.

## Usage

``` r
pdf_report(...)
```

## Arguments

- ...:

  Additional arguments passed to pdf_document.

## Value

R Markdown output format.

## Examples

``` r
if (FALSE) { # \dontrun{
# Add `volker::pdf_report` to the output options of your Markdown document:
#
# ```
# ---
# title: "How to create reports?"
# output: volker::pdf_report
# ---
# ```
} # }
```
