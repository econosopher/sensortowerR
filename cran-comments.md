## New submission

This is the first submission of `sensortowerR` to CRAN.

## Test environments

* local macOS install (ARM64), R 4.2.2
* win-builder (R-devel, R-release) - submitted, awaiting results
* rhub v2 requires GitHub Actions setup - alternative testing performed locally

## R CMD check results

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Phillip Black <pblack@gameeconomistconsulting.com>'
  
  New submission
  
  Size of tarball: 8560994 bytes

* checking installed package size ... NOTE
  installed size is 9.2Mb
  sub-directories of 1Mb or more:
    images   8.9Mb
    
The images are essential for package documentation and examples, showing dashboard outputs and visualizations.

## Previous submission issues fixed

* Fixed invalid URL in DESCRIPTION (now points to correct API docs endpoint)
* Fixed "Lost braces" in documentation by using \verb{} for API endpoints

Thanks!
