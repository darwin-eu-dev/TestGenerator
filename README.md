
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TestGenerator

<!-- badges: start -->
<!-- badges: end -->

Does my cohort is including the correct number and type of patients? Am
I calculating a cohort intersection the right way? Is that the expected
value for treatment duration? It requires just one incorrect parameter
to get incoherent results from a pharmacoepidemiological study, and it
is challenging to test if our calculations are correct on huge
databases.

That is why TestGenerator is useful to push a micro sample of around 10
patients to unit test a study on the OMOP-CDM. It includes tools to
create a blank CDM with a complete vocabulary to perform unit testing on
the results and check if the code is doing what we expect.

This package is based on the unit testing performed on some OHDSI
studies.

## Installation

You can install the development version of TestGenerator from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("darwin-eu-dev/TestGenerator")
```

## Example

The user should create an Excel file with a micro population of around
10 patients for testing purposes. That can include any table from the
OMOP-CDM.

`readPatients()` will read the Excel file, and saves the data in a JSON
file. This is useful if the user wants to create more than one sets of
test populations for testing.

``` r
library(TestGenerator)

TestGenerator::readPatients(here::here("extras", "RSV_Test_Data.xlsx"))
```

`patientCDM()` pushes one of those test populations into a blank CDM
reference.

``` r

cdm <- TestGenerator::patientCDM()
```

Now the user has a CDM reference with a complete vocabulary and a
universe of just 10 patients to unit test functions of a particular
study.
