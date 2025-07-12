
simulate_bias_in_ICSW_with_SL <- function(n_obs, n_groups, SL_library) {
  sim <- simulate_compliance_data(n_obs, n_groups)
  Y <- sim$data$D
  X <- sim$data[, .(X, Z)]
  newX <- data.table(
    X = factor(c(X[, X], X[, X])),
    Z = rep(1:0, each = n_obs))
  sl <- SuperLearner(
    Y, X, newX = newX, family = binomial(),
    SL_library,
    method = "method.AUC",
    verbose = FALSE,
    cvControl = SuperLearner.CV.control(V = 10L, stratifyCV = FALSE,
      shuffle = TRUE, validRows = NULL))
  sim$data[, pred_prob_D_1 := head(sl$SL.predict, n_obs)]
  sim$data[, pred_prob_D_0 := tail(sl$SL.predict, n_obs)]
  sim$data[, compliance_score := pred_prob_D_1 - pred_prob_D_0]
  sim$data[, w := 1 / compliance_score]
  icsw_estimate <-
    sim$data[, sum(Y * Z * w) / sum(Z * w) - sum(Y * (1-Z) * w) / sum((1-Z) * w)] /
    sim$data[, sum(D * Z * w) / sum(Z * w) - sum(D * (1-Z) * w) / sum((1-Z) * w)]
  unname(icsw_estimate - sim$ate)
}

simulate_bias_in_ICSW <- function(n_obs, n_groups) {
  sim <- simulate_compliance_data(n_obs, n_groups)
  logit_model <- glm(D ~ Z * X, binomial, sim$data[, .(X, Z, D, Y)])
  data_Z1 <- data.table(X = sim$data$X, Z = 1)
  data_Z0 <- data.table(X = sim$data$X, Z = 0)
  sim$data[, pred_prob_D_1 := predict(logit_model, data_Z1, type = "response")]
  sim$data[, pred_prob_D_0 := predict(logit_model, data_Z0, type = "response")]
  sim$data[, compliance_score := pred_prob_D_1 - pred_prob_D_0]
  sim$data[, w := 1 / compliance_score]
  icsw_estimate <-
    sim$data[, sum(Y * Z * w) / sum(Z * w) - sum(Y * (1-Z) * w) / sum((1-Z) * w)] /
    sim$data[, sum(D * Z * w) / sum(Z * w) - sum(D * (1-Z) * w) / sum((1-Z) * w)]
  min(2, max(-2, icsw_estimate - sim$ate))
}

plot_ROC_curve <- function(yhat, y) {
  print(plot(performance(
    prediction(yhat, y),
    measure="tpr", x.measure="fpr"
  ), asp = 1, xlim = c(0, 1), ylim = c(0, 1)))
  abline(0, 1, lty = 3)
}


mcreplicate <- function(n, expr, simplify = "array") {
  mcsapply(integer(n), eval.parent(substitute(function(...) expr)),
    simplify = simplify)
}

mcsapply <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
  FUN <- match.fun(FUN)
  answer <- parallel::mclapply(X = X, FUN = FUN, ...)
  if (USE.NAMES && is.character(X) && is.null(names(answer)))
    names(answer) <- X
  if (!isFALSE(simplify) && length(answer))
    simplify2array(answer, higher = (simplify == "array"))
  else answer
}

simulate_compliance_data <- function(n_obs, n_groups) {
  group_level_data <- data.table(
    n_in_group = round(n_obs / n_groups),
    Y0 = rnorm(n_groups, 0, sqrt(.5)),
    mu = sort(rnorm(n_groups, .25, .1)), # pop ATE = .25
    U_always_taker = rev(sort(rlnorm(n_groups))),
    U_never_taker = rev(sort(rlnorm(n_groups))),
    U_complier = sort(3 * rlnorm(n_groups))
  )

  data <- rbindlist(lapply(1:n_groups, function(i) {
    group_level_data[i, .(
      group = i,
      compliance_type = sample(
        c("always taker", "never taker", "complier"),
        n_in_group, replace = TRUE,
        prob = c(U_always_taker, U_never_taker, U_complier))
    )]
  }))
  data <- merge(data,
    group_level_data[, .(group = .I, Y0, mu)],
    by = "group", sort = FALSE)
  data[, `:=`(
    Y0 = Y0 + rnorm(.N, 0, sqrt(.5)),
    mu = mu + rnorm(.N, 0, .05),
    Z = rbinom(n_obs, 1, .5)
  )]
  data[, `:=`(
    Y1 = Y0 + mu
  )]
  data[compliance_type == "always taker", D := 1]
  data[compliance_type == "never taker", D := 0]
  data[compliance_type == "complier", D := Z]
  data[, Y := D * Y1 + (1-D) * Y0]
  list(
    group_level_data = group_level_data,
    data = data[, .(X = factor(group), Z, D, Y, compliance_type)],
    ate = data[,  mean(mu)],
    cates = data[, .(conditional_ate = mean(mu)), compliance_type],
    wald =
      data[, sum(Y * Z) / sum(Z) - sum(Y * (1-Z)) / sum(1-Z)] /
      data[, sum(D * Z) / sum(Z) - sum(D * (1-Z)) / sum(1-Z)]
  )
}

calc_AUROC <- function(predictions, labels) {
  performance(
    prediction(predictions, labels),
    measure = "auc")@y.values[[1]]
}
