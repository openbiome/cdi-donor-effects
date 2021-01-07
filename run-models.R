#!/usr/bin/env Rscript --vanilla

library(tidyverse)
library(lme4)

source("utils.R")

# fixed effects model
fixed_model <- glm(cure ~ delivery + severity, family = "binomial", data = fuf)

# mixed model and confidence intervals
model <- glmer(cure ~ delivery + severity + (1 | partner) + (1 | donor), family = "binomial", data = fuf)
model_ci <- confint(model, oldNames = FALSE)

# test the null hypotheses by dropping donor and partner
nodonor_model <- update(model, cure ~ delivery + severity + (1 | partner))
nopartner_model <- update(model, cure ~ delivery + severity + (1 | donor))

models <- list(
  fixed_model = fixed_model,
  model = model,
  model_ci = model_ci,
  nodonor_model = nodonor_model,
  nopartner_model = nopartner_model
)

write_rds(models, "models.rds")
