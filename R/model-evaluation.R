#' Tools for model evaluation
#'
#' These functions provide tools for evaluating [`trending::trending_model`]s,
#' based on the goodness of fit or on predictive power. [evaluate_aic()]
#' evaluates the goodness of fit of a single model using Akaike's information
#' criterion, measuring the deviance of the model while penalising its
#' complexity. [evaluate_resampling()] uses repeated K-fold cross-validation and
#' the Root Mean Square Error (RMSE) of testing sets to measure the predictive
#' power of a single model. [evaluate_aic()] is faster, but
#' [evaluate_resampling()] is better-suited to select best predicting models.
#' [evaluate_models()] uses either [evaluate_aic()] or [evaluate_resampling()]
#' to compare a series of models.
#'
#' @details These functions wrap around existing functions from several
#'   packages. [stats::AIC()] is used in [evaluate_aic()], and
#'   [evaluate_resampling()] uses [rsample::vfold_cv()] for cross-validation and
#'   [yardstick::rmse()] to calculate RMSE.
#' 
#' @seealso [stats::AIC()] for computing AIC; [rsample::vfold_cv()] for cross
#'   validation; [yardstick::rmse()] for calculating RMSE; `yardstick` also
#'   implements a range of other metrics for assessing model fit outlined at
#'   \url{https://yardstick.tidymodels.org/}; [trending::trending_model()] for
#'   the different ways to build the model objects.
#' 
#' @param model A [trending::trending_model] object.
#' @param data a `data.frame` containing data (including the response variable
#'   and all predictors) used in `model`
#' @param metrics a list of functions assessing model fit, with a similar
#'   interface to [yardstick::rmse()]; see
#'   \url{https://yardstick.tidymodels.org/} for more information
#' @param v the number of equally sized data partitions to be used for K-fold
#'   cross-validation; `v` cross-validations will be performed, each using `v -
#'   1` partition as training set, and the remaining partition as testing set.
#'   Defaults to 1, so that the method uses leave-one-out cross validation, akin
#'   to Jackknife except that the testing set (and not the training set) is used
#'   to compute the fit statistics.
#' @param repeats the number of times the random K-fold cross validation should
#'   be repeated for; defaults to 1; larger values are likely to yield more
#'   reliable / stable results, at the expense of computational time
#' @param models a `list` of models specified as an [trending::trending_model()]
#'   objects.
#' @param method a `function` used to evaluate models: either
#'   [evaluate_resampling()] (default, better for selecting models with good
#'   predictive power) or [evaluate_aic()] (faster, focuses on goodness-of-fit
#'   rather than predictive power)
#' @param ... further arguments passed to the underlying method (e.g. `metrics`,
#'   `v`, `repeats`).
#'
#' @importFrom stats predict
#' @export
#' @rdname evaluate_models
#' @aliases evaluate_resampling
evaluate_resampling <- function(model,
                                data,
                                metrics = list(yardstick::rmse),
                                v = nrow(data),
                                repeats = 1) {
  training_split <- rsample::vfold_cv(data, v = v, repeats = repeats)
  metrics <- do.call(yardstick::metric_set, metrics)
  response <- get_response(model)
  res <- lapply(training_split$splits, function(split) {
    fit <- fit(model, rsample::analysis(split))
    validation <- predict(fit, rsample::assessment(split))
    metrics(validation, validation[[response]], validation$estimate)
  })
  
  res <- do.call(rbind, res)
  res <- tapply(res$.estimate, res$.metric, mean)
  data.frame(metric = row.names(res), score = res)
}


#' @export
#' @rdname evaluate_models
#' @aliases evaluate_aic
evaluate_aic <- function(model, data) {
  full_model_fit <- model$fit(data)
  data.frame(
    metric = "aic",
    score = stats::AIC(full_model_fit$fitted_model)
  )
}

#' @export
#' @rdname evaluate_models
#' @aliases evaluate_models
evaluate_models <- function(models, data, method = evaluate_resampling, ...) {
  ellipsis::check_dots_used()
  out <- lapply(
    models,
    function(model) safely(method)(model, data, ...)
  )
  out <- base_transpose(out)
  out <- tibble::tibble(
    model_name = names(models),
    model = models,
    data = list(data),
    result = out[[1]],
    warning = out[[2]],
    error = out[[3]]
  )
  
  tidyr::unnest(out, "result", keep_empty = TRUE)
}