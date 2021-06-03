context("Population model lavaan")

test_that("function produces correct lavaan syntax for data generation based on specified model", {

  # Opdelen in alle kleine functies, zowel voor bivariaat als trivariaat
  # Elke kleine functie zijn eigen test-that .R-script
  # Bivariate RICLPM with 3 repeated measures ----
  ## Input
  Phi1 <- matrix(c(.5, .1, .4, .5), ncol = 2, byrow = T)
  wSigma1 <- matrix(c(1 , .3, .3, 1) , ncol = 2, byrow = T)
  Psi1 <- matrix(c(0.710, -0.037, -0.037, 0.470), ncol = 2, byrow = T)

  ## Expected
  pop1 <- "RI_A=~1*A1
RI_A=~1*A2
RI_A=~1*A3
RI_B=~1*B1
RI_B=~1*B2
RI_B=~1*B3
RI_A~~1*RI_A
RI_B~~1*RI_B
RI_A~~0.3*RI_B
wA1=~1*A1
wA2=~1*A2
wA3=~1*A3
wB1=~1*B1
wB2=~1*B2
wB3=~1*B3
wA2~0.5*wA1
wA2~0.1*wB1
wB2~0.4*wA1
wB2~0.5*wB1
wA3~0.5*wA2
wA3~0.1*wB2
wB3~0.4*wA2
wB3~0.5*wB2
wA1~~1*wA1
wB1~~1*wB1
wA1~~0.3*wB1
wA2~~0.71*wA2
wA3~~0.71*wA3
wB2~~0.47*wB2
wB3~~0.47*wB3
wA2~~-0.037*wB2
wA3~~-0.037*wB3
A1~~0*A1
A2~~0*A2
A3~~0*A3
B1~~0*B1
B2~~0*B2
B3~~0*B3"

  x <- getLavMod(nK = 2, nT = 3, Phi = Phi, Psi = Psi, wSigma = wSigma, RICov = .3, RIVarProp = .5)
  expect_equal(paste0(paste0(x[,"lhs"], x[,"op"], x[,"pv"], x[,"con"], x[,"rhs"]), collapse = "\n")
               , pop1)

  # Trivariate RICLPM with 4 repeated measures ----
  ## Input
  Phi2 <- matrix(c(.4,  0,  0,
                   .2, .5,  0,
                   .3, .2, .6)
                  , byrow = T, ncol = 3)
  wSigma2 <- matrix(c( 1, .3, .3,
                      .3,  1, .3,
                      .3, .3,  1)
                   , ncol = 3, byrow = T)
  Psi2 <- matrix(c(0.84, 0.16, 0.084,
                   0.16, 0.65, -0.043,
                   0.084, -0.043, 0.294)
                 , ncol = 3, byrow = T)
  RICov2 <- c(.3, .3, .3)

  ## Expected
  pop2 <- "RI_A=~1*A1
RI_A=~1*A2
RI_A=~1*A3
RI_A=~1*A4
RI_B=~1*B1
RI_B=~1*B2
RI_B=~1*B3
RI_B=~1*B4
RI_C=~1*C1
RI_C=~1*C2
RI_C=~1*C3
RI_C=~1*C4
RI_A~~1*RI_A
RI_B~~1*RI_B
RI_C~~1*RI_C
RI_A~~0.3*RI_B
RI_A~~0.3*RI_C
RI_B~~0.3*RI_C
wA1=~1*A1
wA2=~1*A2
wA3=~1*A3
wA4=~1*A4
wB1=~1*B1
wB2=~1*B2
wB3=~1*B3
wB4=~1*B4
wC1=~1*C1
wC2=~1*C2
wC3=~1*C3
wC4=~1*C4
wA2~0.4*wA1
wA2~0*wB1
wA2~0*wC1
wB2~0.2*wA1
wB2~0.5*wB1
wB2~0*wC1
wC2~0.3*wA1
wC2~0.2*wB1
wC2~0.6*wC1
wA3~0.4*wA2
wA3~0*wB2
wA3~0*wC2
wB3~0.2*wA2
wB3~0.5*wB2
wB3~0*wC2
wC3~0.3*wA2
wC3~0.2*wB2
wC3~0.6*wC2
wA4~0.4*wA3
wA4~0*wB3
wA4~0*wC3
wB4~0.2*wA3
wB4~0.5*wB3
wB4~0*wC3
wC4~0.3*wA3
wC4~0.2*wB3
wC4~0.6*wC3
wA1~~1*wA1
wB1~~1*wB1
wC1~~1*wC1
wA1~~0.3*wB1
wA1~~0.3*wC1
wB1~~0.3*wC1
wA2~~0.84*wA2
wA3~~0.84*wA3
wA4~~0.84*wA4
wB2~~0.65*wB2
wB3~~0.65*wB3
wB4~~0.65*wB4
wC2~~0.294*wC2
wC3~~0.294*wC3
wC4~~0.294*wC4
wA2~~0.16*wB2
wA2~~0.084*wC2
wB2~~-0.043*wC2
wA3~~0.16*wB3
wA3~~0.084*wC3
wB3~~-0.043*wC3
wA4~~0.16*wB4
wA4~~0.084*wC4
wB4~~-0.043*wC4
A1~~0*A1
A2~~0*A2
A3~~0*A3
A4~~0*A4
B1~~0*B1
B2~~0*B2
B3~~0*B3
B4~~0*B4
C1~~0*C1
C2~~0*C2
C3~~0*C3
C4~~0*C4"

  x <- getLavMod(nK = 3, nT = 4, Phi = Phi2, Psi = Psi2, wSigma = wSigma2, RICov = RICov2, RIVarProp = .5)
  View(expect_equal(paste0(paste0(x[,"lhs"], x[,"op"], x[,"pv"], x[,"con"], x[,"rhs"]), collapse = "\n")
               , pop2))

})
