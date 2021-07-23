library("DoubleML")

library("MASS")
library("mlr3")
library("mlr3learners")
library("mlr3tuning")

library("foreach")
library("doParallel")
library("doSNOW")

cores <- 64
iters <- 1000

# Parameters
n <- 1000
p <- 200
theta <- 1
gamma <- c(3, 2, 1)
beta <- c(1, 2, 3)

mu <- rep(0, p)
sigma <- diag(p)

experiments_names <- c(
  "seed", "OLS", "Non Orthogonal",
  "Full Sample", "Split Sample",
  "IV-type", "Partialling Out"
)

# Generate Data
data <- list()
for (iter in 1:iters) {
  set.seed(iter)

  x <- mvrnorm(n, mu = mu, Sigma = sigma)
  d <- x[, 1:3] %*% gamma + rnorm(n)
  y <- theta * d + x[, 1:3] %*% beta + rnorm(n)
  data[[iter]] <- data.frame(X = x, d = d, y = y)
}

non_orthogonal_score <- function(y, d, g_hat, m_hat, samples) {
  u_hat <- y - g_hat
  psi_a <- -1 * d * d
  psi_b <- d * u_hat
  psis <- list(psi_a = psi_a, psi_b = psi_b)
  return(psis)
}

# Double Machine Learning

## Double Machine Learing by Lasso
cl <- makeCluster(cores)
registerDoSNOW(cl)
pb <- txtProgressBar(max = iters, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

learner <- lrn("regr.cv_glmnet", nfolds = 5, s = "lambda.min")
ml_g <- learner$clone()
ml_m <- learner$clone()

simulation_lasso <- foreach(
  iter = 1:iters,
  .combine = rbind,
  .options.snow = opts,
  .packages = c("mlr3", "mlr3learners", "DoubleML")
) %dopar% {
  df <- data[[iter]]
  df_dml <- double_ml_data_from_data_frame(df, y_col = "y", d_cols = "d")

  # OLS Estimation
  lrm <- lm(y ~ 1 + ., df)
  theta_ols <- lrm$coefficients["d"]

  # Non-orthogonal Method
  dml_nonorth <- DoubleMLPLR$new(df_dml, ml_g, ml_m,
    n_folds = 2,
    score = non_orthogonal_score, apply_cross_fitting = FALSE
  )
  dml_nonorth$fit()
  theta_nonorth <- dml_nonorth$coef

  # Orthogonal Method with Full Sample
  dml_orth_full <- DoubleMLPLR$new(
    df_dml, ml_g, ml_m,
    n_folds = 1, score = "IV-type", apply_cross_fitting = FALSE
  )
  dml_orth_full$fit()
  theta_orth_full <- dml_orth_full$coef

  # Orthogonal Method with Split Sample
  dml_orth_split <- DoubleMLPLR$new(
    df_dml, ml_g, ml_m,
    n_folds = 2, score = "IV-type"
  )
  dml_orth_split$fit()
  theta_orth_split <- dml_orth_split$coef

  # Double Machine Learning Method by IV type
  dml_iv_type <- DoubleMLPLR$new(
    df_dml, ml_g, ml_m,
    n_folds = 10, score = "IV-type"
  )
  dml_iv_type$fit()
  theta_iv_type <- dml_iv_type$coef

  # Double Machine Learning Method by Partialling Out
  dml_partialling_out <- DoubleMLPLR$new(
    df_dml, ml_g, ml_m,
    n_folds = 10
  )
  dml_partialling_out$fit()
  theta_partialling_out <- dml_partialling_out$coef

  # Return Results
  c(
    iter,
    theta_ols,
    theta_nonorth,
    theta_orth_full,
    theta_orth_split,
    theta_iv_type,
    theta_partialling_out
  )
}
stopCluster(cl)

colnames(simulation_lasso) <- experiments_names
write.csv(
  simulation_lasso,
  file = "../data/intuition_plr_lasso.csv",
  row.names = FALSE
)

## Double Machine Learing by random forest
cl <- makeCluster(cores)
registerDoSNOW(cl)
pb <- txtProgressBar(max = iters, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

learner <- lrn("regr.ranger")
ml_g <- learner$clone()
ml_m <- learner$clone()

param_grid <- list(
  "ml_g" = ParamSet$new(list(
    ParamInt$new("mtry", lower = 2, upper = 25),
    ParamInt$new("num.trees", lower = 50, upper = 200),
    ParamInt$new("max.depth", lower = 2, upper = 10)
  )),
  "ml_m" = ParamSet$new(list(
    ParamInt$new("mtry", lower = 2, upper = 25),
    ParamInt$new("num.trees", lower = 50, upper = 200),
    ParamInt$new("max.depth", lower = 2, upper = 10)
  ))
)
tune_settings <- list(
  terminator = mlr3tuning::trm("evals", n_evals = 20),
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp("cv", folds = 3),
  measure = list(
    "ml_g" = msr("regr.mse"),
    "ml_m" = msr("regr.mse")
  )
)

simulation_rf <- foreach(
  iter = 1:iters,
  .combine = rbind,
  .options.snow = opts,
  .packages = c("mlr3", "mlr3learners", "DoubleML")
) %dopar% {
  df <- data[[iter]]
  df_dml <- double_ml_data_from_data_frame(df, y_col = "y", d_cols = "d")

  # OLS Estimation
  lrm <- lm(y ~ 1 + ., df)
  theta_ols <- lrm$coefficients["d"]

  # Non-orthogonal Method
  dml_nonorth <- DoubleMLPLR$new(df_dml, ml_g, ml_m,
    n_folds = 2,
    score = non_orthogonal_score, apply_cross_fitting = FALSE
  )
  dml_nonorth$fit()
  theta_nonorth <- dml_nonorth$coef

  # Orthogonal Method with Full Sample
  dml_orth_full <- DoubleMLPLR$new(
    df_dml, ml_g, ml_m,
    n_folds = 1, score = "IV-type", apply_cross_fitting = FALSE
  )
  dml_orth_full$fit()
  theta_orth_full <- dml_orth_full$coef

  # Orthogonal Method with Split Sample
  dml_orth_split <- DoubleMLPLR$new(
    df_dml, ml_g, ml_m,
    n_folds = 2, score = "IV-type"
  )
  dml_orth_split$tune(
    param_set = param_grid,
    tune_settings = tune_settings,
    tune_on_folds = FALSE
  )
  dml_orth_split$fit()
  theta_orth_split <- dml_orth_split$coef

  # Double Machine Learning Method by IV type
  dml_iv_type <- DoubleMLPLR$new(
    df_dml, ml_g, ml_m,
    n_folds = 10, score = "IV-type"
  )
  dml_iv_type$tune(
    param_set = param_grid,
    tune_settings = tune_settings,
    tune_on_folds = FALSE
  )
  dml_iv_type$fit()
  theta_iv_type <- dml_iv_type$coef

  # Double Machine Learning Method by Partialling Out
  dml_partialling_out <- DoubleMLPLR$new(
    df_dml, ml_g, ml_m,
    n_folds = 10
  )
  dml_partialling_out$tune(
    param_set = param_grid,
    tune_settings = tune_settings,
    tune_on_folds = FALSE
  )
  dml_partialling_out$fit()
  theta_partialling_out <- dml_partialling_out$coef

  # Return Results
  c(
    iter,
    theta_ols,
    theta_nonorth,
    theta_orth_full,
    theta_orth_split,
    theta_iv_type,
    theta_partialling_out
  )
}
stopCluster(cl)

colnames(simulation_rf) <- experiments_names
write.csv(
  simulation_rf,
  file = "../data/intuition_plr_rf.csv",
  row.names = FALSE
)