# Package index

## Analysis

Functions for describing and analyzing categorical and metric data.
Report functions call table, plot, and effect functions.

- [`report_counts()`](https://strohne.github.io/volker/reference/report_counts.md)
  : Create table and plot for categorical variables
- [`report_metrics()`](https://strohne.github.io/volker/reference/report_metrics.md)
  : Create table and plot for metric variables
- [`tab_counts()`](https://strohne.github.io/volker/reference/tab_counts.md)
  **\[experimental\]** : Output a frequency table
- [`tab_metrics()`](https://strohne.github.io/volker/reference/tab_metrics.md)
  **\[experimental\]** : Output a table with distribution parameters
- [`plot_counts()`](https://strohne.github.io/volker/reference/plot_counts.md)
  **\[experimental\]** : Output a frequency plot
- [`plot_metrics()`](https://strohne.github.io/volker/reference/plot_metrics.md)
  **\[experimental\]** : Output a plot with distribution parameters such
  as the mean values
- [`effect_counts()`](https://strohne.github.io/volker/reference/effect_counts.md)
  **\[experimental\]** : Output effect sizes and test statistics for
  count data
- [`effect_metrics()`](https://strohne.github.io/volker/reference/effect_metrics.md)
  **\[experimental\]** : Output effect sizes and test statistics for
  metric data

## Labeling

Manage and apply variable labels.

- [`codebook()`](https://strohne.github.io/volker/reference/codebook.md)
  : Get variable and value labels from a data set
- [`labs_apply()`](https://strohne.github.io/volker/reference/labs_apply.md)
  **\[experimental\]** : Set column and value labels
- [`labs_clear()`](https://strohne.github.io/volker/reference/labs_clear.md)
  **\[experimental\]** : Remove all comments from the selected columns
- [`labs_store()`](https://strohne.github.io/volker/reference/labs_store.md)
  **\[experimental\]** : Get the current codebook and store it in the
  codebook attribute.
- [`labs_restore()`](https://strohne.github.io/volker/reference/labs_restore.md)
  **\[experimental\]** : Restore labels from the codebook store in the
  codebook attribute.

## Styling

Functions for customized visual styling and formatting.

- [`theme_vlkr()`](https://strohne.github.io/volker/reference/theme_vlkr.md)
  : Define a default theme for volker plots
- [`html_report()`](https://strohne.github.io/volker/reference/html_report.md)
  : Volker style HTML document format
- [`pdf_report()`](https://strohne.github.io/volker/reference/pdf_report.md)
  : Volker style PDF document format

## Data preparation

Functions for data preparation and calculation.

- [`add_index()`](https://strohne.github.io/volker/reference/add_index.md)
  **\[experimental\]** : Calculate the mean value of multiple items
- [`add_clusters()`](https://strohne.github.io/volker/reference/add_clusters.md)
  **\[experimental\]** : Add cluster number to a data frame
- [`add_factors()`](https://strohne.github.io/volker/reference/add_factors.md)
  **\[experimental\]** : Add PCA columns along with summary statistics
  (KMO and Bartlett test) to a data frame

## Example data

- [`chatgpt`](https://strohne.github.io/volker/reference/chatgpt.md) :
  ChatGPT Adoption Dataset CG-GE-APR23
