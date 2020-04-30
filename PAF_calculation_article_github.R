#
# Init ----
#

# Load packages
library(tidyverse)
library(INLA)

#
# Data ----
#

# Run script STEC and animal numbers first

#
# Augment data ----
#

tmp.data.summer <- tmp.data.summer %>% select(-geometry)
ex.data.aug <- bind_rows(
  # Augmentation idicator: 0 = original data, 1 = augmented data
  bind_cols(tmp.data.summer, tibble(augmented = rep(0, nrow(tmp.data.summer)))),
  bind_cols(tmp.data.summer, tibble(augmented = rep(1, nrow(tmp.data.summer))))) %>%
  mutate(
    # Set exposed to reference value
    Kleine_Herkauwers_totaal = if_else(augmented == 1, 0, Kleine_Herkauwers_totaal),
    # Set exposed.cat to reference category
    #exposed.cat = if_else(augmented == 1, factor(levels(exposed.cat)[1], levels = levels(exposed.cat)), exposed.cat),
    # Set outcome to NA
    cases = if_else(augmented == 1, NA_real_, cases)) %>%
  ungroup
ex.data.aug

#
# Fit models ----
#

# Fit Poisson regression model with INLA
fit.inla <- inla(
  formula = cases ~ sex + agecat + log2(Pluimvee_totaal + 1) + log2(Varkens_totaal + 1) +
    log2(Rundvee_totaal + 1) + log2(Kleine_Herkauwers_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = ex.data.aug,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE),
  # Enable posterior sampling
  control.compute = list(config = TRUE))

#
# PAF
#

# Calculate predictions for both the original and augmented data
# for both GLM and INLA models
ex.data.aug <- ex.data.aug %>%
  mutate(
    # Predictions with INLA
    pred.inla = population*exp(fit.inla$summary.linear.predictor[, "mean"]))

# PAF is then given by
pred.data <- ex.data.aug %>%
  group_by(augmented) %>%
  summarize(
    pred.inla = sum(pred.inla))

PAF.inla <- pred.data %>% pull(pred.inla)
PAF.inla <- (PAF.inla[1] - PAF.inla[2])/PAF.inla[1]
PAF.inla

post.inla <- inla.posterior.sample(n = 10000, result = fit.inla)
linpred <- sapply(X = post.inla, FUN = function(x) {
  x.latent <- x$latent
  x.latent[x.latent %>% rownames %>% str_detect(pattern = "Predictor"), ]
})

PAF <- rep(0, ncol(linpred))
for (i in 1:ncol(linpred)) {

  ex.data.aug <- ex.data.aug %>%
    mutate(
      # Predictions with INLA
      pred.inla = population*exp(linpred[, i]))

  # PAF is then given by
  pred.data <- ex.data.aug %>%
    group_by(augmented) %>%
    summarize(
      pred.inla = sum(pred.inla))

  PAF.inla <- pred.data %>% pull(pred.inla)
  PAF.inla <- (PAF.inla[1] - PAF.inla[2])/PAF.inla[1]
  PAF[i] <- PAF.inla
}
quantile(PAF, c(0.5, 0.025, 0.975))
