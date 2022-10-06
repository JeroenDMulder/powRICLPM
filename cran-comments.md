## Resubmission 5
This is a resubmission. In this version I have:

* Deleted the trailing spaces from the description field.

* Included a small data file in the inst/extdata subfolder for use in examples of `summary.powRICLPM()`, `plot.powRICLPM()`, and `give()`. The examples for these functions have now been unwrapped and can be run.  

### R CMD check results

There are 2 NOTES, which can be safely ignored:

* The terms "CLPM" is not misspelled. Moreover, the "(possibly) invalid DOI" is not invalid: The paper it refers to, Mulder (2022), is currently in press but has already been given this DOI. 

* ONLY on win-building: The terms "autoregressive" and "bivariate" are not misspelled. 


## Resubmission 4
This is a resubmission. In this version I have:

* Included "2022" as the year of publication in the description of the DESCRIPTION file. Note, however, that Mulder (2022) is currently in press: It will be published this year, and the given doi is already linked to this paper. 

* Added the missing \value Rd-tags to `powRICLPM_Mplus.RD`, `print.powRICLPM.Rd`, and `summary.powIRCLPM.Rd`. 

* \dontrun{} has been replaced with \donttest{} for the example of `powRICLPM()`: This example takes longer than 5 sec to run but can be executed without error. The examples of `give()` and `plot.powRICLPM()` remain wrapped with \donttest{}: The "out1" object of class "powRICLPM" is missing, thereby leading to an error. The wrapper around the example of `powRICLPM_Mplus()` has been removed as it can be executed without error and has been made to run within 5 sec. 

* The example of `powRICLPM_Mplus()` now writes to tempdir(), and the created files are cleaned up afterwards. 

### R CMD check results

There are 2 NOTES, which can be safely ignored:

* The term "CLPM" is not misspelled. Moreover, the "(possibly) invalid DOI" is not invalid. As mentioned above, the paper it refers to, Mulder (2022), is currently in press but has already been given this DOI. 

* ONLY on Fedora Linux (via R-hub): "checking HTML version of manual ... NOTE Skipping checking HTML validation: no command 'tidy' found." I cannot change that `tidy` is not on the path, or update `tidy` on the external Fedora Linux server.


## Resubmission 3
This is a resubmission. In this version I have:

* Changed the Description field of the DESCRIPTION file such that it now starts with a capital letter (i.e., I deleted the redundant package name in the beginning).


## Resubmission 2
This is a resubmission. In this version I have:

* Changed the Description field of the `powRICLPM()` function such that it now starts with a capital letter (i.e., I deleted the redundant package name in the beginning).

* Used `\doi{}` for DOI's in the `powRICLPM()` documentation. 

* Created a `@references` field for references contained in the `powRICLPM()` documentation.

There were 2 NOTEs, which can be safely ignored:

* "CLPM" and "powRICLPM" are not misspelled words in DESCRIPTION.

* The "(possibly) invalid DOI" refers to the paper detailing the method as implemented in this package. It is currently in press, but once published is available at the included doi.


## Resubmission 1
This is a resubmission. In this version I have:

* Added a trailing slash to the URL in the DESCRIPTION file.

* Extended the Description in the DESCRIPTION file and included a reference about the method implemented in this package. Note that this article is currently in press, but once published online is available at the included doi. 

* Changed an invalid URL in the package startup message.

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* This is a new release.
