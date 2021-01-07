#!/usr/bin/env Rscript --vanilla

library(tidyverse)
library(lme4)
library(broom)
source("utils.R")

# Read in models
models <- read_rds("models.rds")

# Get basic stats about the data --------------------------------------

print("No. data points ~= patients")
print(nrow(fuf))

# Count no. partners
print("No. partners")
fuf$partner %>%
  unique() %>%
  length()

# Count states that partners are in
print("States where partners are")
states <- fuf$partner %>%
  str_sub(1, 2) %>%
  str_to_upper() %>%
  unique() %>%
  keep(~ . %in% c("DC", datasets::state.abb))

print(length(states))
print(states)

# Count no. donors
print("No. donors")
fuf$donor %>%
  unique() %>%
  length()

# Test likelihood ratios for null models ------------------------------

likelihood_ratio_p <- function(null_model, alt_model) {
  G2 <- -2 * (logLik(null_model) - logLik(alt_model))
  pchisq(as.numeric(G2), df = 1, lower.tail = FALSE)
}

donor_p <- likelihood_ratio_p(models$nodonor_model, models$model)
print("Donor p value")
print(donor_p)

partner_p <- likelihood_ratio_p(models$nopartner_model, models$model)
print("Partner p value")
print(partner_p)

# Analyze models ------------------------------------------------------

# Pull the point estimates from the mixed model
estimate <- tidy(models$model) %>%
  filter(group %in% c("partner", "donor")) %>%
  select(group, term, estimate)

# Pull the values from the mixed model confidence intervals
cis <- models$model_ci %>%
  as_tibble(rownames = "term") %>%
  mutate_at("term", ~ str_replace(., "\\|", "."))

results <- left_join(estimate, cis, by = "term") %>%
  select(-term) %>%
  # transform from SDs to variances
  mutate_at(c("estimate", "2.5 %", "97.5 %"), ~ . ** 2)

results

# Get the top 10% performance -----------------------------------------

logit <- function(p) log(p / (1 - p))
invlogit <- function(x) 1 / (1 + exp(-x))

grand_mean <- tidy(models$model) %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

results %>%
  # convert variance->SD
  mutate_if(is.numeric, sqrt) %>%
  mutate(base = 0) %>%
  mutate_if(is.numeric, ~ invlogit(grand_mean + qnorm(0.9) * .)) %>%
  pivot_longer(cols = c(estimate, `2.5 %`, `97.5 %`)) %>%
  mutate(risk_diff = value - base)

# Plot ----------------------------------------------------------------

# Make nicer ordering and labels for data
regroup <- function(df) {
  mutate_at(df, "group", ~ factor(.,
    levels = c("donor", "partner"),
    labels = c("Donor variation", "Site variation")
  ))
}

n_points <- 1e3

plot_data <- results %>%
  pivot_longer(-group) %>%
  mutate(
    data = map(value, ~ tibble(
      p = seq(0, 1, length.out = n_points),
      x = logit(p),
      y_x = dnorm(x, mean = grand_mean, sd = sqrt(.)),
      y = y_x / (p * (1 - p))
    ))
  ) %>%
  unnest(cols = c(data)) %>%
  select(group, name, p, y) %>%
  regroup()

rug_data <- models$model %>%
  # pull donor effects
  ranef() %>%
  as_tibble() %>%
  select(group = grpvar, random_effect = condval) %>%
  # convert to probabilities
  mutate(p = invlogit(grand_mean + random_effect)) %>%
  regroup()

plot <- plot_data %>%
  ggplot(aes(x = p)) +
  facet_grid(rows = vars(group), scales = "free_y") +
  geom_vline(xintercept = invlogit(grand_mean), linetype = 2) +
  geom_line(aes(y = y, group = name, color = name)) +
  geom_rug(
    data = rug_data,
    alpha = 0.25, size = 0.05, length = unit(0.04, "npc")
  ) +
  scale_x_continuous(
    labels = function(x) scales::percent(x, accuracy = 2),
    expand = c(0, 0),
    limits = c(0.5, 1.0)
  ) +
  scale_color_manual(
    breaks = c("estimate", "2.5 %", "97.5 %"),
    values = c("black", rep("gray75", 2))
  ) +
  labs(
    x = "Efficacy",
    y = "Probability density"
  ) +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_line(color = "gray90", linetype = 2)
  )

ggsave("plot.pdf", width = 5, height = 6, units = "in")
