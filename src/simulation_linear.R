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

n <- 1000

theta <- 1
gamma <- c(1, 1, 1)
beta <- c(1, 1, 1)

for (p in c(3, 10, 20, 50, 100, 200)) {
    mu <- rep(0, p)
    sigma <- diag(p)

    # Data Generation
    data <- list()
    for (iter in 1:iters) {
        set.seed(iter)
        x <- mvrnorm(n, mu = mu, Sigma = sigma)
        d <- x[, 1:3] %*% gamma + rnorm(n)
        y <- theta * d + x[, 1:3] %*% beta + rnorm(n)
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
        file = paste("./data/simulation_linear_", p, ".csv", sep = ""),
        row.names = FALSE
    )
}