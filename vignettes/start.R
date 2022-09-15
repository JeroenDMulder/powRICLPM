## ----step2--------------------------------------------------------------------
Phi <- matrix(c(.4, .1, .2, .3), ncol = 2, byrow = T) # The .2 refers to our standardized cross-lagged effect of interest
ICC <- 0.5
RI_cor <- 0.3

## ----analysis, eval = F-------------------------------------------------------
#  output <- powRICLPM(target_power = 0.8,
#                      search_lower = 100,
#                      search_upper = 1000,
#                      search_step = 50,
#                      time_points = c(3, 4, 5),
#                      ICC = ICC,
#                      RI_cor = RI_cor,
#                      Phi = Phi,
#                      within_cor = 0.3,
#                      reps = 1000)

## ----furrr-setup, eval = F----------------------------------------------------
#  # Load the furrr package
#  library(furrr)
#  
#  # Check how many cores are available
#  future::availableCores()
#  
#  # Plan the powRICLPM analysis to run on 1 core less than the number of available cores
#  plan(multisession, workers = 7) # For the case of 8 available cores
#  
#  # Run the powRICLPM analysis
#  with_progress({ # Subscribe to progress updates
#    output <- powRICLPM(target_power = 0.8, # The actual power analysis function
#                        search_lower = 100,
#                        search_upper = 1000,
#                        search_step = 50,
#                        time_points = c(3, 4, 5),
#                        ICC = ICC,
#                        RI_cor = RI_cor,
#                        Phi = Phi,
#                        within_cor = 0.3,
#                        reps = 1000,
#                        parameter = `wB2~wA1`)
#  })
#  
#  # Revert back to sequential execution of code upon completion of the analysis
#  plan(sequential)

## ----summary, eval = F--------------------------------------------------------
#  # 1. General results
#  summary(output)
#  
#  # 2. Get (slightly) more detailed parameter-specific information
#  summary(output, parameter = "wB2~wA1")
#  
#  # 3. Display parameter-specific and condition-specific performance metrics
#  summary(output, sample_size = 400, time_points = 4, ICC = 0.5)
#  

## ----give, eval = F-----------------------------------------------------------
#  # 1. Extract experimental conditions
#  give(output, what = "conditions")
#  
#  # 2. Extract estimation problems
#  give(output, what = "estimation_problems")
#  
#  # 3. Extract results for cross-lagged effect "wB2~wA1"
#  give(output, what = "results", parameter = "wB2~wA1")
#  
#  # 4. Extract parameter names
#  give(output, what = "names")

## ----plot, eval = FALSE-------------------------------------------------------
#  # Create basic plot of powRICLPM object
#  p <- plot(output, parameter = "wB2~wA1")
#  p
#  
#  # Adjust plot aesthetics
#  p2 <- p +
#    labs(title = "Power analysis for RI-CLPM",
#         caption = "Based on 1000 replications.") +
#    scale_x_continuous(name = "Sample size",
#                       breaks = seq(100, 1000, 100),
#                       guide = guide_axis(n.dodge = 2))
#  p2

