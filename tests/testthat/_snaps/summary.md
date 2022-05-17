# powRICLPM_summary() works

    Code
      summary(out1)
    Output
      
      powRICLPM (0.0.0.9003) simulated power for 2 experimental conditions:
      
        No. sample sizes: 300 400
        No. time points: 3
        No. random intercept variance proportions: 0.5
      
        No. Monte Carlo replications: 2
      
      Session info:
      
        No. Monte Carlo replications:  2
        Constraints:  none
        Bounds:  FALSE
      
      Sugested next steps:
      
        - Specify the `parameter` argument of `summary()` to obtain a parameter-specific summary.
        - Specify `names = TRUE` as an argument in `summary()` to obtain names of parameters in the powRICLPM object.
        - Use `coef()` to obtain a table with metrics, across conditions, for a specific parameter.
        - Use `plot()` to visualize results for a specific parameter across conditions.

---

    Code
      summary(out1, parameter = "wB2~wA1")
    Output
      
      powRICLPM (0.0.0.9003) simulated power for 2 experimental conditions:
      
        No. sample sizes: 300 400
        No. time points: 3
        No. random intercept variance proportions: 0.5
      
        No. Monte Carlo replications: 2
        Targeted power: 0.8
      
      Results for wB2~wA1 :
      
        No. conditions with power > targeted power:  0 / 2
      
      Suggested next steps:
      
        - Increase `search_upper` and/or `time_points`, and rerun the analysis.
        - Use `plot()` to visualize results across all experimental conditions.

---

    Code
      summary(out1, names = TRUE)
    Output
       [1] "RI_A~~RI_A" "RI_B~~RI_B" "RI_A~~RI_B" "wA2~wA1"    "wA2~wB1"   
       [6] "wB2~wA1"    "wB2~wB1"    "wA3~wA2"    "wA3~wB2"    "wB3~wA2"   
      [11] "wB3~wB2"    "wA1~~wA1"   "wB1~~wB1"   "wA1~~wB1"   "wA2~~wA2"  
      [16] "wA3~~wA3"   "wB2~~wB2"   "wB3~~wB3"   "wA2~~wB2"   "wA3~~wB3"  

