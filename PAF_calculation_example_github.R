#
# Init ----
#

# Load packages
library(tidyverse)
library(INLA)

#
# Data ----
#

# Some example data
log.baserate <- log(10/100000)
log.RR <- log(2)
ex.data <- tibble(
  exposed = as.double(0:2),
  exposed.cat = factor(exposed),
  population = rep(100000, length(exposed)),
  mu = exp(log(population) + log.baserate + log.RR*exposed),
  cases = rpois(n = length(mu), lambda = mu))

#
# Augment data ----
# Needed for both GLM and INLA
#

ex.data.aug <- bind_rows(
  # Augmentation idicator: 0 = original data, 1 = augmented data
  bind_cols(ex.data, tibble(augmented = rep(0, nrow(ex.data)))),
  bind_cols(ex.data, tibble(augmented = rep(1, nrow(ex.data))))) %>%
  mutate(
    # Set exposed to reference value
    exposed = if_else(augmented == 1, 0, exposed),
    # Set exposed.cat to reference category
    exposed.cat = if_else(augmented == 1, factor(levels(exposed.cat)[1], levels = levels(exposed.cat)), exposed.cat),
    # Set outcome to NA
    cases = if_else(augmented == 1, NA_integer_, cases))
ex.data.aug

#
# Fit models ----
#

# Fit Poisson regression model wiht GLM
fit.glm <- glm(
  #formula = cases ~ exposed.cat,
  formula = cases ~ exposed,
  offset = log(population),
  family = poisson,
  data = ex.data.aug)

# Fit Poisson regression model with INLA
fit.inla <- inla(
  #formula = cases ~ exposed.cat,
  formula = cases ~ exposed,
  E = population,
  family = "poisson",
  data = ex.data.aug,
  # Compute linear predictor
  control.predictor = list(compute = TRUE),
  # Enable posterior sampling
  control.compute = list(config = TRUE))

#
# PAF, the classic way ----
#

# Relative risk
RR <- coef(fit.glm)[-1] %>% exp
RR %>% unname

# Fraction exposed
pE <- ex.data %>%
  mutate(pE = population/sum(population)) %>%
  pull(pE)

# PAF
# https://jech.bmj.com/content/55/7/508?ijkey=88aa3e75467807e3ee61b508ccb5dc29e11ca4a6&keytype2=tf_ipsecsha
PAF <- sum(pE[-1]*(RR - 1))/(1 + sum(pE[-1]*(RR - 1)))
PAF %>% unname

#
# PAF, the alternative way ----
#

# Calculate predictions for both the original and augmented data
# for both GLM and INLA models
ex.data.aug <- ex.data.aug %>%
  mutate(
    # Predictions with GLM
    pred.glm = predict(
      object = fit.glm,
      newdata = ex.data.aug,
      type = "response"),
    # Predictions with INLA
    pred.inla = population*exp(fit.inla$summary.linear.predictor[, "mean"]))

# PAF is then given by
pred.data <- ex.data.aug %>%
  group_by(augmented) %>%
  summarize(
    pred.glm = sum(pred.glm),
    pred.inla = sum(pred.inla))

PAF.glm <- pred.data %>% pull(pred.glm)
PAF.glm <- (PAF.glm[1] - PAF.glm[2])/PAF.glm[1]
PAF.glm

PAF.inla <- pred.data %>% pull(pred.inla)
PAF.inla <- (PAF.inla[1] - PAF.inla[2])/PAF.inla[1]
PAF.inla

