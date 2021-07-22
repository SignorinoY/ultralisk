library("dplyr")
library("tidyr")

library("ggplot2")
library("viridis")
library("latex2exp")

theta <- 1

# LASSO

simulation_lasso <- read.csv("./data/simulation_lasso.csv")

## Non Orthogonal v.s Orthogonal

simulation_temp <- simulation_lasso %>%
  select(seed, Non.Orthogonal, Split.Sample) %>%
  gather(key = Methods, value = theta, c(Non.Orthogonal, Split.Sample))

ggplot(
  data = simulation_temp,
  aes(x = theta, fill = Methods, color = Methods)
) +
  geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
  geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
  scale_color_viridis_d(labels = c("Non Orthogonal", "Orthogonal")) +
  scale_fill_viridis_d(labels = c("Non Orthogonal", "Orthogonal")) +
  ylim(c(0, 150)) +
  labs(x = TeX("$\\theta$"), y = "Count") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, -10, 0)
  )
ggsave(
  "./figures/non-orthogonal-vs-orthogonal-lasso.pdf",
  width = 5, height = 5
)

## Full Sample v.s Split Sample

simulation_temp <- simulation_lasso %>%
  select(seed, Full.Sample, Split.Sample, IV.type) %>%
  gather(
    key = Methods,
    value = theta,
    c(Full.Sample, Split.Sample, IV.type)
  )

ggplot(
  data = simulation_temp,
  aes(x = theta, fill = Methods, color = Methods)
) +
  geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
  geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
  scale_color_viridis_d(labels = c("1-Fold", "10-Folds", "2-Folds")) +
  scale_fill_viridis_d(labels = c("1-Fold", "10-Folds", "2-Folds")) +
  ylim(c(0, 150)) +
  labs(x = TeX("$\\theta$"), y = "Count") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, -10, 0)
  )
ggsave(
  "./figures/full-sample-vs-split-sample-lasso.pdf",
  width = 5, height = 5
)

## IV-type v.s Partialling Out

simulation_temp <- simulation_lasso %>%
  select(seed, IV.type, Partialling.Out) %>%
  gather(key = Methods, value = theta, c(IV.type, Partialling.Out))

ggplot(
  data = simulation_temp,
  aes(x = theta, fill = Methods, color = Methods)
) +
  geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
  geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
 scale_color_viridis_d(labels = c("IV-type", "Partialling Out")) +
 scale_fill_viridis_d(labels = c("IV-type", "Partialling Out")) +
  ylim(c(0, 150)) +
  labs(x = TeX("$\\theta$"), y = "Count") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, -10, 0)
  )

ggsave("./figures/iv-type-vs-partialling-out-lasso.pdf", width = 5, height = 5)

## OLS v.s DML

simulation_temp <- simulation_lasso %>%
  select(seed, OLS, Partialling.Out) %>%
  gather(key = Methods, value = theta, c(OLS, Partialling.Out))

ggplot(
  data = simulation_temp,
  aes(x = theta, fill = Methods, color = Methods)
) +
  geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
  geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
  scale_color_viridis_d(labels = c("OLS", "DML")) +
  scale_fill_viridis_d(labels = c("OLS", "DML")) +
  ylim(c(0, 150)) +
  labs(x = TeX("$\\theta$"), y = "Count") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, -10, 0)
  )

ggsave("./figures/ols-vs-dml-lasso.pdf", width = 5, height = 5)

# Radom Forest

simulation_rf <- read.csv("./data/simulation_rf.csv")

## Non Orthogonal v.s Orthogonal

simulation_temp <- simulation_rf %>%
  select(seed, Non.Orthogonal, Split.Sample) %>%
  gather(key = Methods, value = theta, c(Non.Orthogonal, Split.Sample))

ggplot(
  data = simulation_temp,
  aes(x = theta, fill = Methods, color = Methods)
) +
  geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
  geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
  scale_color_viridis_d(labels = c("Non Orthogonal", "Orthogonal")) +
  scale_fill_viridis_d(labels = c("Non Orthogonal", "Orthogonal")) +
  ylim(c(0, 150)) +
  labs(x = TeX("$\\theta$"), y = "Count") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, -10, 0)
  )

ggsave(
  "./figures/non-orthogonal-vs-orthogonal-rf.pdf",
  width = 5, height = 5
)

## Full Sample v.s Split Sample

simulation_temp <- simulation_rf %>%
  select(seed, Full.Sample, Split.Sample, IV.type) %>%
  gather(
    key = Methods,
    value = theta,
    c(Full.Sample, Split.Sample, IV.type)
  )

ggplot(
  data = simulation_temp,
  aes(x = theta, fill = Methods, color = Methods)
) +
  geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
  geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
  scale_color_viridis_d(labels = c("1-Fold", "10-Folds", "2-Folds")) +
  scale_fill_viridis_d(labels = c("1-Fold", "10-Folds", "2-Folds")) +
  ylim(c(0, 150)) +
  labs(x = TeX("$\\theta$"), y = "Count") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, -10, 0)
  )

ggsave(
  "./figures/full-sample-vs-split-sample-rf.pdf",
  width = 5, height = 5
)

## IV-type v.s Partialling Out

simulation_temp <- simulation_rf %>%
  select(seed, IV.type, Partialling.Out) %>%
  gather(key = Methods, value = theta, c(IV.type, Partialling.Out))

ggplot(
  data = simulation_temp,
  aes(x = theta, fill = Methods, color = Methods)
) +
  geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
  geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
  scale_color_viridis_d(labels = c("IV-type", "Partialling Out")) +
  scale_fill_viridis_d(labels = c("IV-type", "Partialling Out")) +
  ylim(c(0, 150)) +
  labs(x = TeX("$\\theta$"), y = "Count") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, -10, 0)
  )

ggsave("./figures/iv-type-vs-partialling-out-rf.pdf", width = 5, height = 5)

## OLS v.s DML

simulation_temp <- simulation_rf %>%
  select(seed, OLS, Partialling.Out) %>%
  gather(key = Methods, value = theta, c(OLS, Partialling.Out))

ggplot(
  data = simulation_temp,
  aes(x = theta, fill = Methods, color = Methods)
) +
  geom_histogram(position = "identity", binwidth = 0.01, alpha = 0.3) +
  geom_vline(xintercept = theta, linetype = "dotdash", color = "red") +
  scale_color_viridis_d(labels = c("OLS", "DML")) +
  scale_fill_viridis_d(labels = c("OLS", "DML")) +
  ylim(c(0, 150)) +
  labs(x = TeX("$\\theta$"), y = "Count") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, -10, 0)
  )

ggsave(
  "./figures/ols-vs-dml-rf.pdf",
  width = 5, height = 5
)
