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





