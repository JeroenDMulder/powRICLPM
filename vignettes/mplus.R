## ----preparation--------------------------------------------------------------
ICC <- 0.5
RI_cor <- 0.3
Phi <- matrix(c(.4, .1, .2, .3), ncol = 2, byrow = T) # The .2 refers to our standardized cross-lagged effect of interest
wSigma <- matrix(c(1, .3, .3, 1), ncol = 2, byrow = T)

## ----analysis, eval = F-------------------------------------------------------
#  powRICLPM_Mplus(sample_size = 250,
#                  time_points = 4,
#                  ICC = 0.5,
#                  RI_cor = 0.3,
#                  Phi = Phi,
#                  wSigma = wSigma,
#                  reps = 10000,
#                  seed = 123456,
#                  save_path = "./saved")

