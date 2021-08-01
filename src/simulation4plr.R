library("DoubleML")
library("mlr3")
library("mlr3learners")
lgr::get_logger("mlr3")$set_threshold("warn")

library("MASS")
library("foreach")
library("doParallel")
library("doSNOW")

library("logger")
library("glue")

simulation4plr <- function(n = 1000, p = 10, model = "linear3",
                           seed = 0, score = "partialling out") {
  set.seed(seed)

  # Generate Data
  mu <- rep(0, p)
  Sigma <- diag(p)
  X <- mvrnorm(n, mu, Sigma)
  if (model == "linear3") {
    alpha <- c(3, 2, 1)
    beta <- c(1, 2, 3)
    d <- X[, 1:3] %*% alpha + rnorm(n)
    y <- theta * d + X[, 1:3] %*% beta + rnorm(n)
  } else if (model == "linear6") {
    alpha <- c(6, 5, 4, 3, 2, 1)
    beta <- c(1, 2, 3, 4, 5, 6)
    d <- X[, 1:6] %*% alpha + rnorm(n)
    y <- theta * d + X[, 1:6] %*% beta + rnorm(n)
  } else if (model == "relu3") {
    alpha <- c(3, 2, 1)
    beta <- c(1, 2, 3)
    d <- (X[, 1:3] * (X[, 1:3] > 0)) %*% alpha + rnorm(n)
    y <- theta * d + (X[, 1:3] * (X[, 1:3] > 0)) %*% beta + rnorm(n)
  } else if (model == "relu6") {
    alpha <- c(6, 5, 4, 3, 2, 1)
    beta <- c(1, 2, 3, 4, 5, 6)
    d <- (X[, 1:6] * (X[, 1:6] > 0)) %*% alpha + rnorm(n)
    y <- theta * d + (X[, 1:6] * (X[, 1:6] > 0)) %*% beta + rnorm(n)
  } else if (model == "polynomial3") {
    d <- X[, 1]^3 + X[, 2]^2 + X[, 1] * X[, 2] + X[, 2] * X[, 3] + rnorm(n)
    y <- theta * d + X[, 2]^3 + X[, 3]^2 + X[, 1] * X[, 3] +
      X[, 2] * X[, 3] + rnorm(n)
  } else if (model == "polynomial6") {
    d <- rowSums(X[, 1:3])^3 + rowSums(X[, 4:6])^2 + rnorm(n)
    y <- theta * d + rowSums(X[, 1:3])^2 + rowSums(X[, 4:6])^3 + rnorm(n)
  } else if (model == "logistic3") {
    d <- plogis(X[, 1] + X[, 2]) + 0.25 * X[, 3] + rnorm(n)
    y <- theta * d + X[, 1] + 0.25 * plogis(X[, 2] + X[, 3]) + rnorm(n)
  } else if (model == "logistic6") {
    d <- (plogis(X[, 1] + X[, 2]) + plogis(X[, 4] + X[, 5])) +
      0.25 * (X[, 3] + X[, 6]) + rnorm(n)
    y <- theta * d + (X[, 1] + X[, 4]) + 0.25 * (plogis(X[, 2] + X[, 3]) +
      plogis(X[, 5] + X[, 6])) + rnorm(n)
  } else if (model == "indicator3") {
    d <- 1 * (X[, 1] > 3) + 1 * (X[, 2] > 2) + 1 * (X[, 3] > 1) + rnorm(n)
    y <- theta * d + 1 * (X[, 1] > 1) + 1 * (X[, 2] > 2) + 1 * (X[, 3] > 3) +
      rnorm(n)
  } else if (model == "indicator6") {
    d <- 1 * (X[, 1] > 3) + 1 * (X[, 2] > 2) + 1 * (X[, 3] > 1) +
      1 * (X[, 4] > -1) + 1 * (X[, 5] > -2) + 1 * (X[, 6] > -3) +
      1 * (X[, 1] * X[, 6] > 0) + 1 * (X[, 3] * X[, 4] > 1) + rnorm(n)
    y <- theta * d + 1 * (X[, 1] > 1) + 1 * (X[, 2] > 2) + 1 * (X[, 3] > 3) +
      1 * (X[, 4] > -3) + 1 * (X[, 5] > -2) + 1 * (X[, 6] > -1) +
      1 * (X[, 1] * X[, 6] > 2) + 1 * (X[, 3] * X[, 4] > -1) + rnorm(n)
  }

  df <- data.frame(X = X, d = d, y = y)
  df_dml <- double_ml_data_from_data_frame(df, d_cols = "d", y_col = "y")

  # Ordinary Least Squares
  theta_ols <- lm(y ~ 1 + ., data = df)$coefficients["d"]

  # Double Machine Learning with LASSO
  learner <- lrn("regr.cv_glmnet", s = "lambda.min")
  ml_g <- learner$clone()
  ml_m <- learner$clone()
  dml_lasso <- DoubleMLPLR$new(df_dml, ml_g, ml_m, score = score, n_folds = 5)
  dml_lasso$fit()
  theta_dml_lasso <- dml_lasso$coef

  # Double Machine Learning with Random Forest
  learner <- lrn("regr.ranger")
  ml_g <- learner$clone()
  ml_m <- learner$clone()
  dml_rf <- DoubleMLPLR$new(df_dml, ml_g, ml_m, score = score, n_folds = 5)
  dml_rf$fit()
  theta_dml_rf <- dml_rf$coef

  # Return Results
  result <- c(
    model = model,
    n = n,
    p = p,
    score = score,
    seed = seed,
    ols = theta_ols,
    dml.lasso = theta_dml_lasso,
    dml.rf = theta_dml_rf
  )
}


cores <- 100
iters <- 1000
n <- 200
theta <- 1

models <- c(
  "polynomial6", "indicator6"
)
scores <- c("IV-type", "partialling out")
dims <- c(10, 20, 30, 50, 100, 150)

cl <- makeCluster(cores)
registerDoSNOW(cl)
pb <- txtProgressBar(max = iters, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

cat("\n")
for (model in models) {
  for (score in scores) {
    log_info("Start simulation for {model} model with {score} score function:")
    # Parallelization Estimation
    simulation_result <- foreach(
      seed = rep(1:iters, times = 6),
      p = rep(dims, each = iters),
      .combine = rbind,
      .options.snow = opts,
      .packages = c("MASS", "mlr3", "mlr3learners", "DoubleML")
    ) %dopar% simulation4plr(n, p, model, seed, score)

    write.csv(
      simulation_result,
      file = glue("./data/simulation-{model} ({score}).csv"),
      row.names = FALSE
    )
    cat("\n")
  }
}

stopCluster(cl)