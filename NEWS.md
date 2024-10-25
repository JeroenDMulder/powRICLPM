# powRICLPM 0.2.1

## Minor improvements and fixes
* Fixed a bug in version 0.2.0, in which some of the column names of the summary tables from `summary.powRICLPM.parameter()` and `summary.powRICLM.condition()` were misaligned due to a missing column name. 
* Fixed a bug in version 0.2.0, in which population values were not correctly parsed when setting the `constraints = ...` argument of `powRICLPM()` to anything other than `constraints = "none"` (the default). This resulted in incorrect estimates of bias, mean square error (`MSE`), and coverage (`Cover`). Estimates of power, minimum estimate (`Min`), empirical standard deviation (`SD`), average standard error (`SE Avg`) and accuracy were unaffected and thus remained valid.
* Added additional documentation to `summary.powRICLPM()` about the interpretation of information in the summary table. 


# powRICLPM 0.2.0

## Breaking changes
* The function `powRICLPM_Mplus` has been superseded by `powRICLPM()` where users can now set the argument `software = "Mplus"` to use Mplus for their power analysis. The old function has been removed in this version.
* The `bootstrap_reps` argument of `powRICLPM` is now deprecated. Instead, Monte Carlo standard errors are now based on Morris et al. (2017), and available for all performance measures. 
* The argument `alpha` has been superseded by the `significance_criterion` argument. 

## Minor improvements and fixes
* The `reliability` argument can now take in a vector of reliabilities to simulate performance metrics under various levels of item reliability. 
* The `powRICLPM` package now does not import the packages `dplyr` and `purrr` anymore. 
* The `cli` package is now used for error handling. 
* Slight speed and stability improvements when using `software = "lavaan"`. 
* Analysis using `lavaan` now relies on the default `lavTest` settings. Setting `lavTest` to `tests = "none"` lead to errors on some computers. 

# powRICLPM 0.1.1

## Minor improvements and fixes

* Now includes the `check_Phi()` function, which users can use to check if they have specified their `Phi` matrix of lagged-effects as intended. 
* `plot()` now allows other performance measures, such as bias, MSE, coverage, to be plotted through its `y` argument.
* Mistakes in the model syntax of the estimation model when imposing stationarity constraints (using `constraints = "stationarity"`) have now been corrected. 

# powRICLPM 0.1.0

## New features 

* `powRICLPM` can now save the generated data sets by specifying a path with the `save_dat` argument

## Minor improvements and fixes

* The `est_ME` argument in `powRICLPM` has been renamed `estimate_ME`. 
* Internal model fitting using `lavaan` now skips certain checks to speed up the process. 
* The `wSigma` argument in `powRICLPM` has been replaced with the `within_cor` argument. Now, only a `double` denoting the correlation between the within-components needs to specified rather than a correlation matrix. 
* By default, `powRICLPM` now discards results from Monte Carlo replications with inadmissible parameter results, unless bounded estimation is used (`bounds = TRUE`). 



