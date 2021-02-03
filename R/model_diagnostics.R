#' pls205 diagnostics for linear models
#'
#' Makes the diagnostics plots we use in PLS205:
#' qqplots and Scale-Location plots of experimental units
#'
#' @param model A linear model fit with \code{lm} or \code{lmer}.
#' @param EU For models fit with \code{lmer}, the name of the Variable representing the Experimental Units.
#'
#' @return silently, a data.frame with columns for the fitted values, estimated EUs, and standardized residuals
#' @export
#'
pls205_diagnostics = function(model,EU = NULL) {
  # note: for lmer models, be sure to specify the EU that you are interested in!
  if(is(model,'lm')) {
    # diagnostics for lm-type model
    eu_data = data.frame(fitted = fitted(model),EU_std_resid = rstandard(model))
  } else if(is(model,'lmerMod')) {
    if(is.null(EU)) {
      # plot observations
      eu_data = data.frame(fitted = fitted(model),EU_std_resid = resid(model))
    } else {
      # plot estimated values of EU
      if(!EU %in% all.vars(formula(model))) stop(sprintf('Your EU (%s) is not in the model you provided',EU))
      eu_data = data.frame(EU_obs = predict(model,re.form = formula(sprintf('~(1|%s)',EU))),fitted = predict(model,re.form=NA),model@frame)
      eu_data = eu_data[!duplicated(eu_data[[EU]]),]
      eu_data = eu_data[order(eu_data[[EU]]),]
      ranefs = as.data.frame(ranef(model,condVar=T))
      ranefs$condsd = ranefs$condsd/mean(ranefs$condsd)
      eu_data$EU_std_resid = (ranefs$condval/ranefs$condsd)[match(eu_data[[EU]],ranefs$grp)]
    }
  }
  op = par(mfrow=c(1,2))
  eu_data$sq_std_resids = sqrt(abs(eu_data$EU_std_resid))
  car::qqPlot(eu_data$EU_std_resid,main = 'Plot (EU) Normal Q-Q',pch=19,ylab = 'Observed')  # new qqplot function
  plot(eu_data$fitted,eu_data$sq_std_resids,type='n',main = 'Scale-Location',ylab = expression(sqrt(abs(' deviations '))),xlab = 'Fitted values',ylim = c(0,max(eu_data$sq_std_resids)))
  panel.smooth(eu_data$fitted,eu_data$sq_std_resids)
  par(op)
  invisible(eu_data)
}