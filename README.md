
<!-- README.md is generated from README.Rmd. Please edit that file -->

# powRICLPM

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/powRICLPM)](https://CRAN.R-project.org/package=powRICLPM)
[![R-CMD-check](https://github.com/JeroenDMulder/powRICLPM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JeroenDMulder/powRICLPM/actions/workflows/R-CMD-check.yaml)
<a href="https://www.r-pkg.org/pkg/powRICLPM"><img src="https://cranlogs.r-pkg.org/badges/grand-total/powRICLPM" alt="CRAN RStudio mirror downloads"/></a>
<!-- badges: end -->

`powRICLPM` is an R package that aids researchers with performing a
power analysis for the random intercept cross-lagged panel model
(RI-CLPM) by Hamaker, Kuiper, and Grasman (2015), and the Stable Trait
Autoregressive Trait State Model (STARTS) by Kenny and Zautra (1995) and
Kenny and Zautra (2001). It implements the strategy as proposed by
Mulder (2023). Its main functionalities include:

- [**Basic power
  analysis**](https://jeroendmulder.github.io/powRICLPM/articles/start.html):
  Use Monte Carlo simulations to compute the power to reject the
  null-hypothesis (as well as other performance measures such as bias,
  mean square error) for all parameters in the RI-CLPM and STARTS, for a
  specific experimental condition. A condition is defined by its sample
  size, number of repeated measures, proportion of between-unit
  variance, and reliability of the indicators. `powRICLPM` can perform
  power analyses across multiple experimental conditions simultaneously,
  and report the results back in a user-friendly manner.
- [**Extensions**](https://jeroendmulder.github.io/powRICLPM/articles/extensions.html):
  The basic power analysis setup can be extended to include the use of
  bounded estimation, various (stationarity) constraints over time on
  parameters of the estimation model, the generation of nonnormal data,
  among other things.
- [**Mplus**](https://jeroendmulder.github.io/powRICLPM/articles/mplus.html):
  When Mplus is installed, `powRICLPM` can create Mplus syntax, and run
  the power analyses in Mplus.

## Documentation

There are four sources of documentation for `powRICLPM`:

- The rationale for the power analysis strategy underlying this package
  can be found in Mulder (2023).
- Every user-facing function in the package is documented, and the
  documentation can be accessed by running `?function_name` in the R
  console (e.g., `?powRICLPM`). Here, you can find explanations on how
  to use the functions, as well as technical details.
- More elaborate descriptions of this package’s functionality and
  analysis options are described in vignettes. These are accessible via
  the ‘Vignettes’ tab in the menu, or via R using
  `vignette(package = "powRICLPM")`.
- The [FAQ](https://jeroendmulder.github.io/powRICLPM/articles/FAQ.html)
  contains answers to frequently asked question that reach me via email.

## Installation

To install the development version of `powRICLPM`, including the latest
bug fixes and new features, run:

``` r
install.packages("devtools")
devtools::install_github("jeroendmulder/powRICLPM")
```

To install the latest release of `powRICLPM` from CRAN, run:

``` r
install.packages("powRICLPM")
```

## Citing `powRICLPM`

You can cite the R-package with the following citation:

> Mulder, J.D., (2023). Power analysis for the random intercept
> cross-lagged panel model using the powRICLPM R-package. *Structural
> Equation Modeling: A Multidisciplinary Journal, 30*(4), 645-658.
> <https://doi.org/10.1080/10705511.2022.2122467>

## Contact

If you have ideas, comments, or issues you would like to raise, please
get in touch.

- Issues and ideas can be raised on GitHub via
  <https://github.com/jeroendmulder/powRICLPM>
- Pull request can be created on GitHub via
  <https://github.com/jeroendmulder/powRICLPM/pulls>

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-hamaker_critique_2015" class="csl-entry">

Hamaker, Ellen L., Rebecca M. Kuiper, and Raoul P. P. P. Grasman. 2015.
“<span class="nocase">A critique of the cross-lagged panel
model</span>.” *Psychological Methods* 20 (1): 102–16.
<https://doi.org/10.1037/a0038889>.

</div>

<div id="ref-kenny_trait-state-error_1995" class="csl-entry">

Kenny, David A., and Alex Zautra. 1995. “<span class="nocase">The
trait-state-error model for multiwave data</span>.” *Journal of
Consulting and Clinical Psychology1* 63 (1): 52–59.

</div>

<div id="ref-kenny_trait-state_2001" class="csl-entry">

———. 2001. “<span class="nocase">Trait–state models for longitudinal
data</span>.” In *New Methods for the Analysis of Change*, 243–63.
Washington: American Psychological Association.
<https://doi.org/10.1037/10409-008>.

</div>

<div id="ref-mulder_power_2023" class="csl-entry">

Mulder, Jeroen D. 2023. “Power Analysis for the Random Intercept
Cross-Lagged Panel Model Using the powRICLPM r-Package.” *Structural
Equation Modeling: A Multidisciplinary Journal* 30 (4): 645–58.
<https://doi.org/10.1080/10705511.2022.2122467>.

</div>

</div>
