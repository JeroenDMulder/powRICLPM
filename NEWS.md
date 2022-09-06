This file tracks changes to the package.

# powRICLPM 0.1.0

## New features 

* `powRICLPM` can now save the generated data sets by specifying a path with the `save_dat = ""` argument

## Minor improvements and fixes

* The `est_ME` argument in `powRICLPM` has been renamed `estimate_ME`. 
* Internal fitting model fitting using `lavaan` now skips certain checks to speed up the process. 
* The `wSigma` argument in `powRICLPM` has been replaced with the `within_cor` argument. Now, only a `double` denoting the correlation between the within-components needs to specified rather than a correlation matrix. 
* By default, `powRICLPM` now discards results from Monte Carlo replications with inadmissible parameter results, unless bounded estimation is used (`bounds = TRUE`). 

# powRICLPM 0.0.0.9004

## New features

* `powRICLPM()` can now set the reliability of the observed variables for the generated data through the `reliability` argument (i.e., include measurement error).
* `powRICLPM()` can estimate measurement errors by setting `est_ME = TRUE`.
* `powRICLPM()` quantifies the uncertainty around the simulated power through non-parametric bootstrapping. 
* `powRICLPM()` now allows for various estimators implemented in `lavaan`. 
* `give(from = ..., what = "...")` is implemented to extract various bits of information from a `powRICLPM` object.

## Minor improvements and fixes

* `check_N()` now takes imposed constraints into account to create more informative error messages (@dbaranger, #1).
* `summary.powRICLPM()` now tabulates the output.

# powRICLPM 0.0.0.9003

* Original GitHub release of the Beta-version of `powRICLPM` on May 17th, 2022.





