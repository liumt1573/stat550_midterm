library(nlme)
library(ggplot2)
library(dplyr)
library(minpack.lm)
library(MASS)

# ── 0. Data prep ──────────────────────────────────────────────────────────────
dat1 <- read.csv("original.csv")
dat1$Mine        <- factor(dat1$Mine)
dat1$Storage     <- factor(dat1$Storage)
dat1$Media       <- factor(dat1$Media)
dat1$Primary_Key <- factor(dat1$Primary_Key)
dat1$Cu          <- as.numeric(as.character(dat1$Cu))

# ── 1. Model definitions ──────────────────────────────────────────────────────
gompertz <- function(Time, A, mu, lambda) {
  A * exp(-exp((mu * exp(1) / A) * (lambda - Time) + 1))
}

logistic <- function(Time, A, mu, lambda) {
  A / (1 + exp((4 * mu / A) * (lambda - Time) + 2))
}

# ── 2. Heuristic growth classification ───────────────────────────────────────

curve_stats <- dat1 %>%
  group_by(Primary_Key) %>%
  summarise(
    max_OD      = max(OD, na.rm = TRUE),
    min_OD      = min(OD, na.rm = TRUE),
    range_OD    = max_OD - min_OD,
    baseline_OD = mean(OD[Time == min(Time)], na.rm = TRUE),
    fold_change  = max_OD / mean(OD[Time == min(Time)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(grew_data = (range_OD > 0.1) & (fold_change > 1.5))

cat("Heuristic growth:",    sum(curve_stats$grew_data), "\n")
cat("Heuristic no-growth:", sum(!curve_stats$grew_data), "\n")

growth_keys <- curve_stats$Primary_Key[curve_stats$grew_data == TRUE]

# ── 3. Covariates table (one row per curve) ───────────────────────────────────
covariates <- dat1 %>%
  group_by(Primary_Key) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(Primary_Key, Mine, Storage, Media, Cu)

# ── 4. Part 1 — Binary logistic regression (all curves) ──────────────────────
binary_data <- curve_stats %>%
  left_join(covariates, by = "Primary_Key") %>%
  mutate(grew = as.integer(grew_data))

growth_model <- glm(grew ~ Mine + Storage + Cu,
                    data   = binary_data,
                    family = binomial)
summary(growth_model)

# Odds ratios
or_table <- data.frame(
  term    = names(coef(growth_model)),
  OR      = exp(coef(growth_model)),
  lower   = exp(confint(growth_model)[, 1]),
  upper   = exp(confint(growth_model)[, 2]),
  p_value = summary(growth_model)$coefficients[, 4]
)
print(or_table)

# McFadden R2
cat("McFadden R2:", round(1 - (growth_model$deviance / growth_model$null.deviance), 3), "\n")

# Confusion matrix
predicted_class <- as.integer(fitted(growth_model) > 0.5)
conf_mat <- table(predicted = predicted_class, actual = binary_data$grew)
print(conf_mat)
cat("Accuracy:", round(sum(diag(conf_mat)) / sum(conf_mat), 3), "\n")

# Plot predicted probability of growth
pred_data <- expand.grid(
  Cu      = seq(0, 1200, by = 10),
  Mine    = levels(binary_data$Mine),
  Storage = levels(binary_data$Storage)
)
pred_data$Mine    <- factor(pred_data$Mine,    levels = levels(binary_data$Mine))
pred_data$Storage <- factor(pred_data$Storage, levels = levels(binary_data$Storage))

preds <- predict(growth_model, newdata = pred_data, type = "response", se.fit = TRUE)
pred_data$prob  <- preds$fit
pred_data$lower <- preds$fit - 1.96 * preds$se.fit
pred_data$upper <- preds$fit + 1.96 * preds$se.fit

ggplot(pred_data, aes(x = Cu, y = prob, color = Mine, fill = Mine)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  facet_wrap(~ Storage, labeller = labeller(Storage = c(BB = "Bio Bank", CR = "Cold Room"))) +
  labs(
    title = "Probability of growth by copper concentration and mine",
    x     = "Copper concentration (ppm)",
    y     = "P(growth)",
    color = "Mine",
    fill  = "Mine"
  ) +
  theme_bw()
# ── 5. Part 2 — Bounded per-curve Gompertz/Logistic fits (growth curves only) ─
# ── Stage 1: bounded per-curve Gompertz fits (growth curves only) ─────────────
dat1_growth <- dat1[dat1$Primary_Key %in% growth_keys, ]

fit_gompertz_bounded <- function(df) {
  tryCatch({
    fit <- nlsLM(
      OD ~ gompertz(Time, A, mu, lambda),
      data    = df,
      start   = list(A = max(df$OD, na.rm = TRUE), mu = 0.1, lambda = median(df$Time) / 2),
      lower   = c(A = 1e-6, mu = 1e-6, lambda = 0),
      upper   = c(A = 10,   mu = 10,   lambda = max(df$Time)),
      control = nls.lm.control(maxiter = 200)
    )
    as.data.frame(t(coef(fit)))
  }, error = function(e) data.frame(A = NA, mu = NA, lambda = NA))
}

params_gom <- dat1_growth %>%
  group_by(Primary_Key) %>%
  group_modify(~ fit_gompertz_bounded(.x)) %>%
  ungroup()

cat("Gompertz failed:", sum(is.na(params_gom$A)), "of", nrow(params_gom), "\n")

# ── Stage 2: plain OLS on extracted parameters ────────────────────────────────
stage2_data <- params_gom %>%
  left_join(covariates, by = "Primary_Key") %>%
  filter(!is.na(A), !is.na(mu), !is.na(lambda),
         A > 0, mu > 0, lambda > 0)

cat("Stage 2 input:", nrow(stage2_data), "curves\n")

lm_A      <- lm(A      ~ Mine + Storage + Cu, data = stage2_data)
lm_mu     <- lm(mu     ~ Mine + Storage + Cu, data = stage2_data)
lm_lambda <- lm(log(lambda) ~ Mine + Storage + Cu, data = stage2_data)

cat("\n── Two-stage: A (max OD) ──\n");       print(summary(lm_A))
cat("\n── Two-stage: mu (growth rate) ──\n"); print(summary(lm_mu))
cat("\n── Two-stage: lambda (lag time) ──\n");print(summary(lm_lambda))

# ── 7. Diagnostic plots for Stage 2 fits ─────────────────────────────────────
plot_stage2_diagnostics <- function(results, label) {
  par(mfrow = c(3, 2))
  for (param in c("A", "mu", "lambda")) {
    m <- results[[param]]
    plot(fitted(m), resid(m), main = paste(label, param, "- Residuals vs Fitted"),
         xlab = "Fitted", ylab = "Residuals"); abline(h = 0, col = "red")
    qqnorm(resid(m), main = paste(label, param, "- QQ Plot")); qqline(resid(m), col = "red")
  }
  par(mfrow = c(1, 1))
}

plot_stage2_diagnostics(results_gom, "Gompertz")


# ── NLME using bounded nlsLM estimates as starting values ─────────────────────

# Only use growth curves
dat1_growth$Primary_Key <- factor(dat1_growth$Primary_Key)
dat2_growth <- groupedData(OD ~ Time | Primary_Key, data = dat1_growth)

# Get population-level starting values from the nlsLM fits
pop_starts <- params_gom %>%
  filter(!is.na(A), !is.na(mu), !is.na(lambda),
         A > 0, mu > 0, lambda > 0) %>%
  summarise(A = median(A), mu = median(mu), lambda = median(lambda))

cat("Population starts — A:", pop_starts$A, " mu:", pop_starts$mu, " lambda:", pop_starts$lambda, "\n")

ctrl <- nlmeControl(
  maxIter      = 500,
  msMaxIter    = 500,
  tolerance    = 1e-3,
  pnlsTol      = 0.1,
  returnObject = TRUE
)

# Start simple — random effect on A only, no covariates
m_gom_re <- nlme(
  OD ~ gompertz(Time, A, mu, lambda),
  data   = dat2_growth,
  fixed  = A + mu + lambda ~ 1,
  random = A ~ 1 | Primary_Key,
  start  = c(A = pop_starts$A, mu = pop_starts$mu, lambda = pop_starts$lambda),
  control = ctrl
)
summary(m_gom_re)

# If that converges, add covariates to fixed effects
m_gom_cov <- nlme(
  OD ~ gompertz(Time, A, mu, lambda),
  data   = dat2_growth,
  fixed  = list(
    A      ~ Mine + Storage + Cu,
    mu     ~ Mine + Storage + Cu,
    lambda ~ Mine + Storage + Cu
  ),
  random = A + mu ~ 1 | Primary_Key,
  start  = c(
    A      = pop_starts$A, 0, 0, 0,
    mu     = pop_starts$mu, 0, 0, 0,
    lambda = pop_starts$lambda, 0, 0, 0
  ),
  control = ctrl
)
summary(m_gom_cov)

start_stage <- fixef(m_gom_cov)

start_stage


m_gom_full <- nlme(
  OD ~ gompertz(Time, A, mu, lambda),
  data   = dat2_growth,
  fixed  = list(
    A      ~ Mine + Storage + Cu,
    mu     ~ Mine + Storage + Cu,
    lambda ~ Mine + Storage + Cu
  ),
  random = A + mu + lambda ~ 1 | Primary_Key,
  start  = start_stage,
  control = ctrl
)


# Compare models
anova(m_gom_re, m_gom_cov)

# Check residual distribution
qqnorm(residuals(m_gom_cov, type = "normalized"))
qqline(residuals(m_gom_cov, type = "normalized"), col = "red")

# Are random effects normally distributed?
re <- ranef(m_gom_cov)

# QQ plot of random effects for A
qqnorm(re[, "A.(Intercept)"], main = "Random effects for A")
qqline(re[, "A.(Intercept)"], col = "red")

# Plot residuals vs fitted
plot(m_gom_cov)



# ── Comparison table: two-stage vs NLME ───────────────────────────────────────
compare_coefs <- function(lm_obj, nlme_obj, param_prefix, param_label) {
  lm_coefs <- summary(lm_obj)$coefficients
  nlme_coefs <- summary(nlme_obj)$tTable

  # Extract matching rows from NLME
  nlme_rows <- nlme_coefs[grepl(param_prefix, rownames(nlme_coefs)), ]

  cat("\n══", param_label, "══\n")
  cat("\n── Two-stage (OLS) ──\n");  print(round(lm_coefs, 6))
  cat("\n── NLME ──\n");             print(round(nlme_rows, 6))
}

compare_coefs(lm_A,      m_gom_cov, "^A\\.",      "Max OD (A)")
compare_coefs(lm_mu,     m_gom_cov, "^mu\\.",     "Growth rate (mu)")
compare_coefs(lm_lambda, m_gom_cov, "^lambda\\.", "Lag time (lambda)")
