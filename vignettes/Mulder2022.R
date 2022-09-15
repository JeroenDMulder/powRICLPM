## ----setup, eval = FALSE, echo=FALSE------------------------------------------
#  library(powRICLPM)
#  library(ggplot2)
#  library(furrr)
#  library(progressr)

## ----preliminary-analysis, eval = FALSE---------------------------------------
#  # Matrix of standardized lagged effects
#  Phi <- matrix(c(0.20, 0.10, 0.15, 0.30), byrow = FALSE, ncol = 2)
#  
#  # powRICLPM automatically computes Psi based on Phi and within_cor
#  
#  # Setup parallel processing to speed up computations
#  plan(multisession, workers = 6)
#  
#  # Perform preliminary power analysis (with progress bar)
#  with_progress({
#    out_preliminary <- powRICLPM(target_power = 0.8,
#                                 search_lower = 200,
#                                 search_upper = 2000,
#                                 search_step = 100,
#                                 time_points = c(3, 4, 5),
#                                 ICC = c(0.3, 0.5, 0.7),
#                                 RI_cor = 0.35,
#                                 Phi = Phi,
#                                 within_cor = 0.26,
#                                 reps = 100,
#                                 seed = 123456)
#  })
#  
#  # Tabular summary of results
#  summary(out_preliminary)
#  res_wB2wA1 <- give(out_preliminary, what = "results", parameter = "wB2~wA1")
#  
#  # Visualize power
#  p <- plot(x = out_preliminary, parameter = "wB2~wA1")
#  
#  # Tailor visualization for Mulder (under review)
#  p <- p +
#    labs(color = "Number of time points") +
#    scale_x_continuous(name = "Sample size",
#                       breaks = seq(200, 2000, 200),
#                       guide = guide_axis(n.dodge = 2)) +
#    theme(legend.position = "bottom")
#  p
#  ggsave("Mulder2022_preliminary_power.png", height = 5, width = 7)

## ----validation, eval = FALSE-------------------------------------------------
#  # Setup parallel processing to speed up computations
#  plan(multisession, workers = 6)
#  
#  # Perform preliminary power analysis (with progress bar)
#  with_progress({
#    out_validation <- powRICLPM(target_power = 0.8,
#                                search_lower = 900,
#                                search_upper = 1800,
#                                search_step = 100,
#                                time_points = c(4, 5),
#                                ICC = c(0.3, 0.5, 0.7),
#                                RI_cor = 0.35,
#                                Phi = Phi,
#                                within_cor = 0.26,
#                                reps = 2000,
#                                seed = 123456)
#  })
#  
#  # Tabular summary of results
#  summary(out_validation, parameter = "wB2~wA1")
#  res_wB2wA1 <- give(out_validation, what = "results", parameter = "wB2~wA1")
#  
#  # Visualize power
#  p2 <- plot(out_validation, parameter = "wB2~wA1")
#  
#  # Tailor visualization of power for Mulder (2022)
#  p2 <- p2 +
#    labs(color = "Number of time points") +
#    scale_x_continuous(name = "Sample size",
#                       breaks = seq(900, 1800, 100),
#                       guide = guide_axis(n.dodge = 2)) +
#    scale_color_manual(values = c("#00BA38", "#619CFF")) +
#    theme(legend.position = "bottom")
#  p2
#  ggsave("Mulder2022_validation_power.png", height = 5, width = 7)

