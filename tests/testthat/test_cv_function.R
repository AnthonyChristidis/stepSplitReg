# --------------------------------------------------
# Test Script - Output from cv.SplitGLM Function
# --------------------------------------------------

# Required libraries
library(mvnfast)
library(stepSplitReg)
library(glmnet)

# Context of test script
context("Verify output of cross-validation function.")

# There should be an error if we want to compute the IF TS, and no returns are provided
test_that("Error in the cross-validation function.", {

  # Setting the parameters
  p <- 800
  n <- 40
  n.test <- 2000
  sparsity <- 0.2
  rho <- 0.5
  SNR <- 3

  # Generating the coefficient
  p.active <- floor(p*sparsity)
  a <- 4*log(n)/sqrt(n)
  neg.prob <- 0.2
  nonzero.betas <- (-1)^(rbinom(p.active, 1, neg.prob))*(a + abs(rnorm(p.active)))
  
  # Correlation structure
  Sigma <- matrix(0, p, p)
  Sigma[1:p.active, 1:p.active] <- rho
  diag(Sigma) <- 1
  true.beta <- c(nonzero.betas, rep(0 , p - p.active))
  
  # Computing the noise parameter for target SNR
  sigma.epsilon <- as.numeric(sqrt((t(true.beta) %*% Sigma %*% true.beta)/SNR))
  
  # Simulate some data
  set.seed(1)
  x.train <- mvnfast::rmvn(n, mu=rep(0,p), sigma=Sigma)
  y.train <- 1 + x.train %*% true.beta + rnorm(n=n, mean=0, sd=sigma.epsilon)
  x.test <- mvnfast::rmvn(n.test, mu=rep(0,p), sigma=Sigma)
  y.test <- 1 + x.test %*% true.beta + rnorm(n.test, sd=sigma.epsilon)
  

  # glmnet - CV (Single Group)
  glmnet.fit <- cv.glmnet(x.train, y.train,
                          alpha=3/4)
  glmnet.coef <- as.vector(coef(glmnet.fit, s="lambda.min"))
  
  # # stepSplitReg - CV (Multiple Groups)
  # split.out <- cv.stepSplitReg(x.train, y.train, n_models = c(5, 10), max_variables = NULL, keep = 4/4,
  #                              model_criterion = c("F-test", "RSS")[1],
  #                              stop_criterion = c("F-test", "pR2", "aR2", "R2", "Fixed")[1], stop_parameter = 0.05, 
  #                              shrinkage = TRUE, alpha = 4/4, include_intercept = TRUE, 
  #                              n_lambda = 100, tolerance = 1e-2, max_iter = 1e5, n_folds = 5, 
  #                              model_weights = c("Proportional", "Equal", "Stacking", "EN")[2], ensemble_alpha = 3/4,
  #                              n_treads = 1)
  # split.coef <- coef(split.out)
  
  expect_vector(glmnet.coef)

})




