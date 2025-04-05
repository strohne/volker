## R CMD check results

0 errors | 0 warnings | 0 notes

* This is the release of version 3.1.0
* No reverse dependencies found.
* No issues in the current CRAN Package Check Results.
* We import 21 packages because our package is used
  to genenerate reports based on established other packages.

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


## R CMD check results

0 errors | 0 warnings | 0 notes

* This is the release of version 3.0.0
* No reverse dependencies found.
* No issues in the current CRAN Package Check Results.
* We import 21 packages because our package is a package
  used to genenerate reports based on established other packages.
* Checking with win-builder produces one Note:
  The introduction vignette is large because it includes plots. 
  This is a package for generating reports, the plots are included
  for documentation purposes.
  
## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is the release of version 2.1.0
* No reverse dependencies found.
* Added missing package anchors to \link{} targets
  (noted in the CRAN Package Check Results)
* We import 21 packages because our package is developed on top
  of established statistical procedures and reporting function.
  Implementing the functions ourselves would decrease reproducebility
  and increase the risk of errors.
* The introduction vignette is large because it includes plots. 
  This is a package for generating reports, the plots are included
  for documentation purposes.


## R CMD check results

0 errors | 0 warnings | 0 notes

* This is the release of version 2.0.1
* Removed the rendered skeleton.html from the markdown template folder
  (it sneaked into the last release, in consequence folders instead of single
  files were created)
* No reverse dependencies found.
* No issues in the CRAN Package Check Results
  
## R CMD check results

0 errors | 0 warnings | 0 notes

* This is the release of version 2
* No reverse dependencies found
* Removed non-ASCII characters from the example dataset 
  (as they were noted in the CRAN Package Check Results for linux fedora)

## Resubmission
This is a resubmission. In this version I have:

* In the examples for html_report(), commented out the failing dontrun section content
* Replaced all occurences of T/F  by TRUE/FALSE  
* Added the missing return value documentation for idx_add()
* Removed the zip_tables()-function from the exports and removed its example containing :::
* Additionally, reorganized the help index page (removed functions to be called by parent functions as they are linked in the documentation, resulting in a more concise documentation)

Thank you for your unvaluable work. I'm sorry for the oversights. 
Although reading lots of CRAN related materials, obviously, I still missed some important points.

## Resubmission
This is a resubmission. In this version I have:

* As requested: Fixed invalid file URIs by removing links from README.md  
* In addition: Proof read docstrings a second time and fixed some typos  
* In addition: Revised plot height calculation  
  
## R CMD check results

0 errors | 0 warnings | 1 note

* This is the first release.
* Because of the package type, tests and vignettes include rendered plots, that makes the package large
* Several features are drafted for next versions, you'll find some unused code and todo comments, for example preparing topic modeling

