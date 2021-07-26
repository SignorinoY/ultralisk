dml_plr_lasso_rf <- function(df) {
    df_dml <- double_ml_data_from_data_frame(df, y_col = "y", d_cols = "d")

    # Ordinary Least Squares
    model_lm <- lm(y ~ 1 + ., data = df)
    theta_ols <- model_lm$coefficients["d"]

    # Double Machine Learning with LASSO
    learner <- lrn("regr.cv_glmnet", nfolds = 3, s = "lambda.min")
    ml_g <- learner$clone()
    ml_m <- learner$clone()
    dml_lasso <- DoubleMLPLR$new(
        df_dml, ml_g, ml_m,
        n_folds = 5
    )
    dml_lasso$fit()
    theta_dml_lasso <- dml_lasso$coef

    # Double Machine Learning with Random Forest
    learner <- lrn("regr.ranger")
    ml_g <- learner$clone()
    ml_m <- learner$clone()
    param_grid <- list(
        "ml_g" = ParamSet$new(list(
            ParamInt$new("num.trees", lower = 128, upper = 1024),
            ParamInt$new("max.depth", lower = 4, upper = 256)
        )),
        "ml_m" = ParamSet$new(list(
            ParamInt$new("num.trees", lower = 128, upper = 1024),
            ParamInt$new("max.depth", lower = 4, upper = 256)
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

    dml_rf <- DoubleMLPLR$new(
        df_dml, ml_g, ml_m,
        n_folds = 5
    )
    dml_rf$tune(
        param_set = param_grid,
        tune_settings = tune_settings,
        tune_on_folds = TRUE
    )
    dml_rf$fit()
    theta_dml_rf <- dml_rf$coef

    # Return Results
    theta_hat <- c(theta_ols, theta_dml_lasso, theta_dml_rf)
    return(theta_hat)
}