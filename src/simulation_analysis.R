library("dplyr")
library("tidyr")

library("ggplot2")
library("viridis")
library("latex2exp")

theta <- 1

# Linear Models

simulation_linear_200 <- read.csv("./data/simulation_linear_200.csv")
simulation_linear_200$seed <- 1:1000

simulation_temp <- simulation_linear_200 %>%
    gather(key = Methods, value = theta, c(OLS, DML.LASSO, DML.RF))

ggplot(
    data = simulation_temp,
    aes(x = theta, fill = Methods, color = Methods)
) +
    geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
    geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
    scale_color_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
    ) +
    scale_fill_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
    ) +
    ylim(c(0, 190)) +
    labs(x = TeX("$\\theta$"), y = "Count") +
    theme_minimal() +
    theme(
        legend.position = "top",
        legend.margin = margin(0, 0, -10, 0)
    )

ggsave(
    "./reports/figures/simulation-linear-200.pdf",
    width = 5, height = 5
)


simulation_linear_100 <- read.csv("./data/simulation_linear_100.csv")
simulation_linear_100$seed <- 1:1000

simulation_temp <- simulation_linear_100 %>%
    gather(key = Methods, value = theta, c(OLS, DML.LASSO, DML.RF))

ggplot(
    data = simulation_temp,
    aes(x = theta, fill = Methods, color = Methods)
) +
    geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
    geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
    scale_color_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
    ) +
    scale_fill_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
    ) +
    ylim(c(0, 190)) +
    labs(x = TeX("$\\theta$"), y = "Count") +
    theme_minimal() +
    theme(
        legend.position = "top",
        legend.margin = margin(0, 0, -10, 0)
    )

ggsave(
    "./reports/figures/simulation-linear-100.pdf",
    width = 5, height = 5
)


simulation_linear_50 <- read.csv("./data/simulation_linear_50.csv")
simulation_linear_50$seed <- 1:1000

simulation_temp <- simulation_linear_50 %>%
    gather(key = Methods, value = theta, c(OLS, DML.LASSO, DML.RF))

ggplot(
    data = simulation_temp,
    aes(x = theta, fill = Methods, color = Methods)
) +
    geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
    geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
    scale_color_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
    ) +
    scale_fill_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
    ) +
    ylim(c(0, 190)) +
    labs(x = TeX("$\\theta$"), y = "Count") +
    theme_minimal() +
    theme(
        legend.position = "top",
        legend.margin = margin(0, 0, -10, 0)
    )

ggsave(
    "./reports/figures/simulation-linear-50.pdf",
    width = 5, height = 5
)


simulation_linear_20 <- read.csv("./data/simulation_linear_20.csv")
simulation_linear_20$seed <- 1:1000

simulation_temp <- simulation_linear_20 %>%
    gather(key = Methods, value = theta, c(OLS, DML.LASSO, DML.RF))

ggplot(
    data = simulation_temp,
    aes(x = theta, fill = Methods, color = Methods)
) +
    geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
    geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
    scale_color_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
    ) +
    scale_fill_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
    ) +
    ylim(c(0, 190)) +
    labs(x = TeX("$\\theta$"), y = "Count") +
    theme_minimal() +
    theme(
        legend.position = "top",
        legend.margin = margin(0, 0, -10, 0)
    )

ggsave(
    "./reports/figures/simulation-linear-20.pdf",
    width = 5, height = 5
)


simulation_linear_10 <- read.csv("./data/simulation_linear_10.csv")
simulation_linear_10$seed <- 1:1000

simulation_temp <- simulation_linear_10 %>%
    gather(key = Methods, value = theta, c(OLS, DML.LASSO, DML.RF))

ggplot(
    data = simulation_temp,
    aes(x = theta, fill = Methods, color = Methods)
) +
    geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
    geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
    scale_color_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
    ) +
    scale_fill_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
    ) +
    ylim(c(0, 190)) +
    labs(x = TeX("$\\theta$"), y = "Count") +
    theme_minimal() +
    theme(
        legend.position = "top",
        legend.margin = margin(0, 0, -10, 0)
    )

ggsave(
    "./reports/figures/simulation-linear-10.pdf",
    width = 5, height = 5
)


simulation_linear_3 <- read.csv("./data/simulation_linear_3.csv")
simulation_linear_3$seed <- 1:1000

simulation_temp <- simulation_linear_3 %>%
    gather(key = Methods, value = theta, c(OLS, DML.LASSO, DML.RF))

ggplot(
    data = simulation_temp,
    aes(x = theta, fill = Methods, color = Methods)
) +
    geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
    geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
    scale_color_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
    ) +
    scale_fill_viridis_d(
        labels = c("DoubleML (LASSO)", "DoubleML (RF)", "OLS")
    ) +
    ylim(c(0, 190)) +
    labs(x = TeX("$\\theta$"), y = "Count") +
    theme_minimal() +
    theme(
        legend.position = "top",
        legend.margin = margin(0, 0, -10, 0)
    )

ggsave(
    "./reports/figures/simulation-linear.pdf",
    width = 5, height = 5
)
# Polynomial Models
