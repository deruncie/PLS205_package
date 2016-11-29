#' Add a model predictions and SE band to a data.frame
#'
#' Adds columns y_pred, ymax and ymin to a data.frame based on predictions from a previously fitted
#' linear model (lm or lmer). Note: lmer models don't provide SEs, so those values are set to NA.
#'
#' @inheritParams ggplot2::stat_identity
#' @param data A data.frame object with all terms used in the model
#' @param fitted_model a model object (ex from lm or lmer) compatible with the `predict` function
#' @param SEs width of confidence interval band (in se's)
#' @return dataframe augmented with columns \code{y_pred}, \code{ymin}, and \code{ymax}
#' @export
add_intervals = function(data,fitted_model,SEs = 2){
  if(inherits(fitted_model,'merMod')){  # no SE from predict of lmer model
    data$y_pred = predict(fitted_model,newdata = data)
    data$ymax = data$ymin = NA
  } else{
    pred_data = predict(fitted_model,newdata = data,se.fit = T)
    data$y_pred = pred_data$fit
    data$ymax = data$y_pred + SEs*pred_data$se.fit
    data$ymin = data$y_pred - SEs*pred_data$se.fit
  }
  data
}
