# Introduction 
A shiny app to provider a user friendly way to QA data files using centralised code. This is deployed via the DfE visual studio and rsconnect subscriptions. There are three environments, all accessible to DfE AD:

- Production - https://rsconnect/rsc/dfe-published-data-qa
- Pre-production - https://rsconnect-pp/rsc/dfe-published-data-qa
- Development - https://rsconnect-pp/rsc/dev-dfe-published-data-qa

Code used to create functions and variables called by the global, server, and UI scripts can be found in the 'R' folder.

## Packages
Package control is handled using renv. You may need to run `renv::restore()` if this is your first time using the project.

## Tests
Automated tests have been created using testthat and shinytest. All functions created to screen files with have an associated failing file, and unit test using testthat, as well as a couple of other unit tests of functions for the app. A helper function is used to easily input the associated file and test against it. Utilise this when writing any new checks - create the example file first, followed by the unit test, followed by the function. You may then need to update the UI tests if new tests create differences in the outputted page.

UI tests have been created using shinytest that test the app loads, that the reset button really does clear everything, and that files that we'd expect to hit each particular stage of the screening do so. More should be added over time as extra features are added.

## Helpful notes
The function `run_tests()` is created in the Rprofile script and is available in the console at all times to run both the unit and ui tests.

The function `tidy_code()` is created in the Rprofile script and therefore is always available in the console to tidy code according to tidyverse styling using the styler package.

R/profilingCode.r contains the outline of code to use when profiling the app. Use the lines in here to read in data when developing using the console.

In general all .r files will have a usable outline, so make use of that for navigation if in RStudio: Ctrl-Shift-O.
