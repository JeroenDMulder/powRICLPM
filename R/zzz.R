.onAttach <- function(libname, pkgname) {

  # Subscribe to progress updates from anywhere
  # progressr::handlers(global = TRUE)

  # Display startup messages
  packageStartupMessage(
    paste0(
      "\nLoaded powRICLPM (", utils::packageVersion("powRICLPM"), "):\n",
      "\n  Author: Jeroen D. Mulder",
      "\n  Documentation: https://jeroen.d.mulder.github.io/powRICLPM",
      "\n  Questions & issues: https://github.com/JeroenDMulder/powRICLPM/issues\n"
    )
  )
}
