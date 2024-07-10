[![automatedTests](https://github.com/dfe-analytical-services/dfe-published-data-qa/actions/workflows/automatedTests.yaml/badge.svg?branch=master)](https://github.com/dfe-analytical-services/dfe-published-data-qa/actions/workflows/automatedTests.yaml) [![deployShinyApps](https://github.com/dfe-analytical-services/dfe-published-data-qa/actions/workflows/deployShinyApps.yaml/badge.svg?branch=master)](https://github.com/dfe-analytical-services/dfe-published-data-qa/actions/workflows/deployShinyApps.yaml)

<h1 align="center">
  <br>
  DfE published data quality assurance 
  <br>
</h1>

<p align="center">
  <a href="#introduction">Introduction</a> |
  <a href="#requirements">Requirements</a> |
  <a href="#how-to-use">How to use</a> |
  <a href="#data-sources">Data sources</a> |
  <a href="#contact">Contact</a>
</p>

---

## Introduction 

A shiny app to provide a user friendly way to QA data files using centralised code. This is deployed via the DfE visual studio and rsconnect subscriptions. There are three environments only accessible to DfE AD:

- Production - https://rsconnect/rsc/dfe-published-data-qa
- Pre-production - https://rsconnect-pp/rsc/dfe-published-data-qa
- Development - https://rsconnect-pp/rsc/dev-dfe-published-data-qa

There is also an instance hosted on shinyapps.io, which is externally accessible:

- External - https://department-for-education.shinyapps.io/dfe-published-data-qa

**Note**

Sensitive data should not be screened on the shinyapps.io version, as we do not have control over this hosting server. A warning modal will appear to double check this with you before screening any files. If you're external to DfE and want to screen sensitive files, you can run a version of the app locally, see the [software requirements](#requirements) and [how to use](#how-to-use) sections below.

Code used to create functions and variables called by the global, server, and UI scripts can be found in the 'R' folder.

---

## Requirements


### i. Software requirements (for running locally)

- Installation of R Studio 1.2.5033 or higher

- Installation of R 3.6.2 or higher

- Installation of RTools40 or higher

### ii. Programming skills required (for editing or troubleshooting)

- R at an intermediate level, [DfE R training guide](https://dfe-analytical-services.github.io/r-training-course/)

  - Particularly [R Shiny](https://shiny.rstudio.com/)
  
---

## How to use

### Running the app locally

1. Clone or download the repo. 

2. Open the R project in R Studio.

3. Run `renv::restore()` to install dependencies.

4. Run `shiny::runApp()` to run the app locally.

### Packages

Package control is handled using renv. As in the steps above, you will need to run `renv::restore()` if this is your first time using the project.

### Tests

Automated tests have been created using testthat and shinytest2. These tests can be ran locally using `shinytest2::test_app()`. 

All functions created to screen files have an associated failing file, and unit test using testthat, all sharing the same name. There are also a couple of other unit tests of functions in the app. A helper function is used to easily input the associated file and test against it; utilise this when writing any new checks - create the example file first, followed by the unit test, followed by the function. You may then need to update the UI tests if new tests create differences in the outputted page. This is expected due to the snapshot based nature of shinytest2.

End to end UI tests have been created using shinytest2 that test the app loads, that the reset button really does clear everything, and that files that we'd expect to hit each particular stage of the screening do so.

GitHub Actions provide CI by running the automated tests and checks for code styling, with badges in this README referring to the latest state of the master branch. The yaml files for these workflows can be found in the `.github/workflows` folder.

### Deployment

For the internal versions, deployment is handled via the dfe-gov-uk azure devops instance, and deployed to the DfE rsconnect servers.

For the external instance, the app is deployed to the department's shinyapps.io subscription using GitHub actions. The yaml file for this can be found in the `.github/workflows` folder.

### Navigation

In general all .r files will have a usable outline, so make use of that for navigation if in RStudio: `Ctrl-Shift-O`.

### Profiling

`R/manual_scripts/profilingCode.r` contains the outline of code to assist when profiling the app. It is commented out by default it isn't ran when loading the app.

### Debugging

There is a debugging script (named as such) in the `R/manual_scripts` folder. This uses the functions `readFile()` and `screenFiles()` to allow you to debug what is happening by running the core screening functions in the console, useful if screening a particular file crashes the app in an unknown way. This also usually brings up the traceback in RStudio, which often helps to pin down the culprit line of code. It is commented out by default it isn't ran when loading the app.

### Code styling

The function `styler::style_dir()` should be used to tidy code according to tidyverse styling using the styler package. This function also helps to test the running of the code and for basic syntax errors such as missing commas and brackets.

There is a GitHub action workflow that automatically runs this against PRs to ensure all code going into the main branch is styled appropriately.

---

## Data sources

### Geographies

The standard geographies used in this tool are sourced from (or developed with) the Office for National Statistics (ONS). As such, they are maintained in line with the ONS's [open geography portal](https://geoportal.statistics.gov.uk/). 

#### Updating ward_pcon_lad_la or lad_lsip lookups

There's an R script in the `R/manual_scripts/` folder containing functions to update both the geography lookups. To use this, download the latest version of the file from the searches linked in the comments of that script, and then save a copy of that data in a new folder called `/data/downloaded_source_data/`. The functions will then append any new data from that CSV into the lookup files.

#### Local skills improvement plan area (LSIP)

These were developed in direct communications with the ONS ([ONS Geography e-mail](mailto:ONS.Geography@ons.gov.uk)) and they provided the basis for the code and name file used in this tool. At the time of writing, this has not been added to the open geography portal, although boundary files have been ([LSIP boundary map files](https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=name&tags=all(BDY_LSIP%2CAUG_2023))).

---

## Contact

explore.statistics@education.gov.uk
