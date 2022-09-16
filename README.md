
<!-- README.md is generated from README.Rmd. Please edit that file -->

# powRICLPM

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/powRICLPM)](https://CRAN.R-project.org/package=powRICLPM)
[![R-CMD-check](https://github.com/JeroenDMulder/powRICLPM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JeroenDMulder/powRICLPM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`powRICLPM` is an `R` package that performs a power analysis for the
random intercept cross-lagged panel model (RI-CLPM) in a simple and
user-friendly way. It implements the strategy as proposed by Mulder (in
press). Its main functionalities include:

-   [Setting up and performing a basic power
    analysis](https://jeroendmulder.github.io/powRICLPM/articles/start.html):
    Obtain the power to reject the null-hypothesis of no effect (as well
    as other performance measures, such as bias, mean square error,
    etc.) for all parameters in the RI-CLPM given a specific sample
    size, number of repeated measures, and proportion of between-unit
    variance (among other things). This can be done across multiple
    experimental conditions simultaneously (i.e., across varying numbers
    of repeated measures, proportions of between-unit variance, etc.).
-   [Extending the basic power analysis
    setup](https://jeroendmulder.github.io/powRICLPM/articles/extensions.html):
    This includes the option to perform bounded estimation, impose
    various (stationarity) constraints over time on parameters of the
    estimation model, and include the estimation of measurement error.
-   [Create Mplus model
    syntax](https://jeroendmulder.github.io/powRICLPM/articles/mplus.html):
    Create syntax for performing a RI-CLPM power analysis using Mplus.

## Documentation

There are four sources of `powRICLPM` documentation:

-   The rationale for the power analysis strategy implemented here can
    be found in Mulder (in press).
-   Every user-facing function in the package is documented, and the
    documentation can be accessed by running `?function_name` in the R
    console, e.g., `?powRICLPM`. These function references contain
    explanations on how to use the functions, as well as technical
    details.
-   There are four main vignettes (accessible via the ‘Vignettes’ tab),
    describing functionalities and analysis options of this package more
    generally. The ‘Example’ vignette serves as the supplementary
    material to Mulder (in press), and contains the R code for an
    illustrative example using the `powRICLPM` package.
-   The
    [FAQ](https://jeroendmulder.github.io/powRICLPM/articles/FAQ.html)
    contains answers to frequently asked question that reach me via
    email.

## Installation

You can install the development version of `powRICLPM` from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("jeroendmulder/powRICLPM")
```

## Citing `powRICLPM`

You can cite the R-package with the following citation:

> Mulder, J.D., (in press). *Power analysis for the random intercept
> cross-lagged panel model using the powRICLPM R-package*

## Contact

If you have ideas, comments, or issues you would like to raise, please
get in touch.

-   Issues and ideas can be raised on GitHub via
    <https://github.com/jeroendmulder/powRICLPM>
-   Pull request can be created on GitHub via
    <https://github.com/jeroendmulder/powRICLPM/pulls>
