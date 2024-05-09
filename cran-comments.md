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

