## ----setup, eval = FALSE------------------------------------------------------
#  library(powRICLPM)
#  library(ggplot2)
#  library(furrr)
#  library(progressr)

## ----preliminary-analysis, eval = FALSE---------------------------------------
#  # Matrix of standardized lagged effects
#  Phi <- matrix(c(0.20, 0.10, 0.15, 0.30), byrow = FALSE, ncol = 2)
#  
#  # Correlation matrix of within-components
#  wSigma <- matrix(c(1, 0.26, 0.26, 1), byrow = FALSE, ncol = 2)
#  
#  # powRICLPM automatically computes Psi based on Phi and wSigma
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
#                                 ICC = c(0.50, 0.55, 0.60),
#                                 RI_cor = 0.35,
#                                 Phi = Phi,
#                                 wSigma = wSigma,
#                                 reps = 5,
#                                 seed = 20220209)
#  })
#  
#  # Tabular summary of results
#  summary(out_preliminary)
#  coef_wB2wA1 <- coef_powRICLPM(out_preliminary, parameter = "wB2~wA1")
#  coef_wB2wA1 # Can be used to check if any issues occurred with estimation
#  
#  # Visualize power
#  p <- plot_powRICLPM(object = out_preliminary,
#                      x = "sample_size",
#                      y = "pwr",
#                      color = "factor(time_points)",
#                      wrap = "ICC",
#                      parameter = "wB2~wA1")
#  
#  # Tailor visualization for Mulder (2022)
#  p <- p +
#    labs(color = "Number of time points") +
#    scale_x_continuous(name = "Sample size",
#                       breaks = seq(200, 2000, 200),
#                       guide = guide_axis(n.dodge = 2)) +
#    theme(legend.position = "bottom")
#  p
#  #ggsave("Mulder2022_preliminary_power.png", height = 5, width = 7)
#  
#  # Visualize accuracy
#  a <- plot_powRICLPM(out_preliminary,
#                      "sample_size",
#                      "acc",
#                      color = "factor(time_points)",
#                      wrap = "ICC",
#                      parameter = "wB2~wA1")
#  a <- a +
#    labs(color = "Number of time points") +
#    ylab("Accuracy") +
#    scale_x_continuous(name = "Sample size",
#                       breaks = seq(200, 2000, 200),
#                       guide = guide_axis(n.dodge = 2)) +
#    theme(legend.position = "bottom")
#  a
#  #ggsave("Mulder2022_preliminary_accuracy.png", height = 5, width = 7)
#  

## ----validation, eval = FALSE-------------------------------------------------
#  # Setup parallel processing to speed up computations
#  plan(multisession, workers = 5)
#  
#  # Perform preliminary power analysis (with progress bar)
#  with_progress({
#    out_validation <- powRICLPM(target_power = 0.8,
#                                search_lower = 1200,
#                                search_upper = 2000,
#                                search_step = 100,
#                                time_points = c(4, 5),
#                                ICC = c(0.50, 0.55, 0.60),
#                                RI_cor = 0.35,
#                                Phi = Phi,
#                                wSigma = wSigma,
#                                reps = 2000,
#                                seed = 20220209)
#  })
#  
#  # Tabular summary of results
#  summary(out_validation, parameter = "wB2~wA1")
#  coef_wB2wA1 <- coef_powRICLPM(out_validation, parameter = "wB2~wA1")
#  coef_wB2wA1
#  
#  # Visualize power
#  p2 <- plot_powRICLPM(out_validation,
#                      "sample_size",
#                      "pwr",
#                      color = "factor(time_points)",
#                      wrap = "ICC",
#                      parameter = "wB2~wA1")
#  
#  # Tailor visualization of power for Mulder (2022)
#  p2 <- p2 +
#    labs(color = "Number of time points") +
#    scale_x_continuous(name = "Sample size",
#                       breaks = seq(1200, 2000, 100),
#                       guide = guide_axis(n.dodge = 2)) +
#    scale_color_manual(values = c("#00BA38", "#619CFF")) +
#    theme(legend.position = "bottom")
#  p2
#  #ggsave("Mulder2022_validation_power.png", height = 5, width = 7)
#  
#  # Visualize accuracy
#  a2 <- plot_powRICLPM(out_validation,
#                      "sample_size",
#                      "acc",
#                      color = "factor(time_points)",
#                      wrap = "ICC",
#                      parameter = "wB2~wA1")
#  a2 <- a2 +
#    labs(color = "Number of time points") +
#    scale_x_continuous(name = "Sample size",
#                       breaks = seq(1200, 2000, 100),
#                       guide = guide_axis(n.dodge = 2)) +
#    theme(legend.position = "bottom")
#  a2
#  #ggsave("Mulder2022_validation_accuracy.png", height = 5, width = 7)
#  

