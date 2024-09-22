save_condition_Mplus <- function(condition, save_path) {

  # Create connection object
  path <- file.path(save_path, paste0(
    "condition", condition$condition_id,".inp"
  )) |>
    normalizePath(mustWork = FALSE)

  # Write content to file
  writeLines(condition$Mplus_synt, path)

  invisible()
}
