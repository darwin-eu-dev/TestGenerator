
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TestGenerator

<!-- badges: start -->
<!-- badges: end -->

Creates tests on patient data for DARWIN EU studies. It accepts an Excel
file with a sample of patients, and tests on an empty CDM if the results
are correct from a particular study package.

## Installation

You can install the development version of TestGenerator from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("darwin-eu-dev/TestGenerator")
```

## Example

Function to read the .xls file with data generally from 10 patients. By
default, it generates a JSON files in the inst/testCases folders.

``` r
library(TestGenerator)

TestGenerator::readPatients(here::here("extras", "RSV_Test_Data.xlsx"))
```

Data then can be pushed to a blank CDM with the complete vocabulary.

``` r

cdm <- TestGenerator::patientCDM()
```
