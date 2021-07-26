library("DoubleML")

library("MASS")
library("mlr3")
library("mlr3learners")
library("mlr3tuning")

library("foreach")
library("doParallel")
library("doSNOW")

source("./src/dml_plr_lasso_rf.R")

cores <- 100
iters <- 1000

# Data Generation

n <- 1000
p <- 3
mu <- rep(0, p)
sigma <- diag(p)

theta <- 1

data <- list()
for (iter in 1:iters) {
    set.seed(iter)
    x <- mvrnorm(n, mu = mu, Sigma = sigma)
    d <- plogis(x[, 1] + x[, 2]) + x[, 3] + rnorm(n)
    y <- theta * d + x[, 1] + x[, 2] + plogis(x[, 3]) + rnorm(n)
    data[[iter]] <- data.frame(X = x, d = d, y = y)
}

# Parallelization Estimation

cl <- makeCluster(cores)
registerDoSNOW(cl)
pb <- txtProgressBar(max = iters, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

simulation_result <- foreach(
    iter = 1:iters,
    .combine = rbind,
    .options.snow = opts,
    .packages = c("mlr3", "mlr3learners", "DoubleML", "mlr3tuning")
) %dopar% {
    df <- data[[iter]]
    theta_hat <- dml_plr_lasso_rf(df)
}

stopCluster(cl)

colnames(simulation_result) <- c("OLS", "DML.LASSO", "DML.RF")
write.csv(
    simulation_result,
    file = "./data/simulation_logis.csv",
    row.names = FALSE
)