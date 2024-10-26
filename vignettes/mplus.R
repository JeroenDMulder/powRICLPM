## ----preparation, eval = FALSE------------------------------------------------
#  Phi <- matrix(c(.4, .1, .2, .3), ncol = 2, byrow = T)
#  # The .2 refers to our standardized cross-lagged effect of interest
#  ICC <- 0.5
#  RI_cor <- 0.3

## ----analysis, eval = F-------------------------------------------------------
#  powRICLPM(
#    search_lower = 100,
#    search_upper = 1000,
#    search_step = 50,
#    time_points = c(3, 4, 5),
#    ICC = 0.5,
#    RI_cor = 0.3,
#    Phi = Phi,
#    within_cor = 0.3,
#    reps = 10000,
#    seed = 123456,
#    save_path = "./saved"
#    software = "Mplus"
#  )

