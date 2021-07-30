library("dplyr")
library("tidyr")
library("glue")

library("ggplot2")
library("viridis")
library("latex2exp")

theta <- 1

models <- c(
  "linear3", "linear6",
  "relu3", "relu6",
  "polynomial3", "polynomial6",
  "logistic3", "logistic6",
  "indicator3", "indicator6"
)
scores <- c("IV-type", "partialling out")

for (model in models) {
  for (score in scores) {
    simulation_result <- read.csv(
      glue("./data/simulation-{model} ({score}).csv")
    )
    simulation_result <- simulation_result %>%
      gather(key = methods, value = theta, c(ols.d, dml.lasso.d, dml.rf.d))
    ggplot(
      data = simulation_result,
      mapping = aes(x = theta, fill = methods, color = methods)
    ) +
      geom_histogram(bins = 50, position = "identity", alpha = 0.3) +
      geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
      facet_wrap(. ~ p, ncol = 3) +
      scale_color_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
      ) +
      scale_fill_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
      ) +
      labs(x = TeX("$\\theta$"), y = "Count") +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.margin = margin(0, 0, -10, 0)
      )
    ggsave(
      glue("./reports/figures/simulation-{model} ({score}).pdf"),
      width = 8, height = 6
    )
  }
}