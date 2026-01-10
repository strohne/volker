# volkeR-Package: High-level functions for tabulating, charting and reporting survey data.

## Description

The volkeR package provides high-level functions for rapidly creating
reports from survey data. It is designed for social science workflows
that require fast and easy generation of descriptive statistics,
visualizations, and reproducible reports.

## Use Cases

Descriptive analysis of survey data. Researchers can use
[`report_counts()`](https://strohne.github.io/volker/reference/report_counts.md)or
[`report_metrics()`](https://strohne.github.io/volker/reference/report_metrics.md)
to generate summary tables and plots and optionally perform effect size
calculations for key variables.

## Input Data

VolkeR takes R data frames (data.frame, tibble) as input, containing
categorical and metric variables.

**Required input**

- An R data frame containing the variables to be analyzed.
- A column selection specifying at least one variable.

**Optional input**

- Grouping variables for comparisons.
- Additional arguments controlling summary statistics, effect size
  calculations, or plotting behavior.

Example data are available as the `chatgpt` data frame with survey
responses on ChatGPT usage.

``` r
data <- volker::chatgpt
```

## Output Data

The main functions of volkeR,
[`report_counts()`](https://strohne.github.io/volker/reference/report_counts.md)
and
[`report_metrics()`](https://strohne.github.io/volker/reference/report_metrics.md),
produce a volker report object. The output typically includes:

- **A volkeR tibble**  
  A structured table containing summary statistics such as frequencies
  and percentages for categorical variables or means, standard
  deviations, and sample sizes for metric variables.

- **A ggplot2 plot object**  
  A visualization of the same information displayed in the table.  
  The plot can be printed, saved, or further customized like any
  standard ggplot object.

Optional components include:

- effect-size tables (e.g. Pearson’s r, test statistics)
- statistical model summaries (when model estimation is enabled)
- additional diagnostic or comparison plots

## Hardware Requirements

No GPU or special hardware is required.

## Environment Setup

With R installed:

``` r
install.packages("volker")
```

## How to Use

To apply volkeR to the [example input](#input-data) and generate the
[example output](#output-data), proceed as follows:

``` r
library(volker)
data <- chatgpt

# Create your first table and plot, counting answers to an item battery
report_counts(data, starts_with("cg_adoption_social"))

# Create your first table and plot, reporting mean values of the item battery
report_metrics(data, starts_with("cg_adoption_social"))
```

To customize the analysis, users can:

- Select different variables or variable groups, for example:
  `report_counts(data, sd_gender)`.
- Add grouping variables for comparisons, for example:
  `report_metrics(data, starts_with("cg_adoption_social"), sd_gender)`.
- Enable or disable effect size calculations, for example:
  `report_metrics(data, starts_with("cg_adoption_social"), sd_gender, effect = TRUE).`

Beyond its main reporting functions, volkeR also supports:

- Factor and cluster analysis
- Statistical modeling (regression and analysis of variance)
- Reliability analysis for content analysis
- Labeling features to handle messy data outputs (variable and value
  names)

For a comprehensive introduction, please refer to the [package
overview](https://strohne.github.io/volker/articles/introduction.html).
Further options, examples and documentation can be found in the [help
pages](https://strohne.github.io/volker/) and
[vignettes](https://github.com/strohne/volker/tree/main/vignettes).

## Technical Details

See the package documentation on
[CRAN](https://cran.r-project.org/web/packages/volker/index.html "CRAN")
for details.

## Acknowledgements

The volker package is inspired by outputs used in the textbook [Einfache
Datenauswertung mit R](https://doi.org/10.1007/978-3-658-34285-2)
(Gehrau & Maubach et al., 2022), which provides an introduction to
univariate and bivariate statistics and data representation using
RStudio and R Markdown.

## Disclaimer

## Contact Details

**Authors**

- Jakob Jünger (University of Münster)
- Henrieke Kotthoff (University of Münster)

**Contributors**

- Chantal Gärtner (University of Münster)
- Sophia Rinne (University of Münster)

Issue Tracker: <https://github.com/strohne/volker/issues>
