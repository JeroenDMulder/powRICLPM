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



