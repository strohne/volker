# volker 3.3.1

* Updated tests to take into account the latest changes in `bind_rows()`.
* Handle duplicate labels

# volker 3.3.0

* Support formulas in model functions
* Removed skimr dependency

# volker 3.2.0

* Implemented linear modeling in `add_model()`
  including effect sizes, standardised betas and  adjusted p values
* Implemented reliability calculation and classification performance indicators
  in `agree_tab()`
* Implemented cooccurrence analysis: `report_counts()` generates 
  heatmaps (tiles-parameter) and npmi values (method-parameter).
* Option to keep missings if possible by setting `options(vlkr.na.omit=FALSE)`
  (pairwise instead of listwise handling of missings)

# volker 3.1.0

* SPSS labels are now supported
* In plot functions, the width of bars and columns 
  is calculated from data (similar to mosaic plots)
* The tables now have a row for totals
* Improved error messages
* `model_metrics_tab()` and `model_metrics_plot()` 
  provide a first draft of lm analysis


# volker 3.0.0

* Implemented cluster and factor analysis

# volker 2.1.0

* Implemented missing plot, tab, and effect functions
* Implemented PDF and Word rendering
* Improve handling of residual values and respective configuration options

# volker 2.0.1
* Fixed markdown template

# volker 2.0.0

* Added effect size calculation and statistical tests
* Added theme support for colors and other design elements
* Reorganised parameters
* Revised documentation

# volker 1.0.2

* Fixed documentation and coding style issues

# volker 1.0.1

* Revised plot height calculation  
* Removed links to unused vignettes from the readme.md  
* Fixed some typos  

# volker 1.0.0

* Removed deprecated beta functions. The beta version is freezed in the beta branch on GitHub.
* Removed tests that rely on system settings (plot rendering and HTML line endings)
* Initial CRAN submission.
