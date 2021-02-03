StatPlotlm2 <- ggplot2::ggproto("StatPlotlm2", ggplot2::Stat,
  required_aes = c("x", "y"),

  compute_group = function(data, scales, fitted_model = NULL,se = TRUE,
                           n = 100,
                           fullrange = FALSE,
                           SEs = 2
                           ) {
    factor_columns = sapply(data,function(x) !is.numeric(x))
    factor_levels = lapply(data[,factor_columns],unique)
    x_column = sapply(colnames(data),function(x) !x %in% c('colour','group','PANEL') & all(data[[x]] == data$x))
    if(sum(x_column) > 1) x_column[['x']] = F
    x_column = which(x_column)[1]  # choose first column that is identical to x

    if(!factor_columns[x]) {
      # this means the x_column is continuous, so we want n points along the range of x

    }
    recover()


    # creates a data.set expanded over all levels of factors in data and with a continuous x-covariate spread over n points
    # do some work to figure out which columns are factors and which are numeric.
    factor_columns = sapply(data,function(x) !is.numeric(x))
    factor_levels = lapply(data[,factor_columns],unique)
    factor_grid = expand.grid(factor_levels)
    x_column = sapply(colnames(data),function(x) !x %in% c('colour','group','PANEL') & all(data[[x]] == data$x))
# recover()
    if(sum(x_column) > 1) x_column[['x']] = F
    if(any(!factor_columns[x_column])){
      # this means the x_column is continuous, so we want n points along the range of x
      if(fullrange){
        # the range is full range of the facet
        min = scales$x$dimension()[1]
        max = scales$x$dimension()[2]
      } else {
        # the range is just the range of the data
        min <- min(data$x,na.rm=T)
        max <- max(data$x,na.rm=T)
      }
      # repeate x_seq for each combination of factor_levels
      new_data = c(data.frame(x = seq(min,max,length = n)),factor_levels)
      new_data = expand.grid(new_data)
      new_data[[colnames(data)[x_column][1]]] = new_data$x
    } else{
      # this means x_column is a factor, so no extra interpolation is needed
      combined_factor_levels = apply(data[,factor_columns],1,paste,collapse='::')
      new_data = data[tapply(1:length(combined_factor_levels),combined_factor_levels,function(x) x[1]),factor_columns]
      # new_data = expand.grid(factor_levels)
      new_data$x = new_data[[colnames(data)[x_column][1]]]
      new_data = new_data[order(new_data$x),]
    }
    if(inherits(fitted_model,'merMod')){
      # no SE from predict of lmer model
      new_data$y_pred = predict(fitted_model,newdata = new_data)
      new_data$ymax = new_data$ymin = new_data$y_pred
    } else{
      # can use SEs from lm fit
      pred_data = predict(fitted_model,newdata = new_data,se.fit = T)
      new_data$y_pred = pred_data$fit
      if(se) {
        new_data$ymax = new_data$y_pred + SEs*pred_data$se.fit
        new_data$ymin = new_data$y_pred - SEs*pred_data$se.fit
      }
    }
    new_data$y = new_data$y_pred
    new_data
  }
)

#' Add a trend line and SE ribbon for a model fit object
#'
#' This is a replacement for \code{stat_smooth} where you can supply your own model fit (lm or lmer)
#' to be plotted as a trend line.
#'
#' The main difference from stat_smooth, or other geom_ of stat_ functions is that you must add
#' all of the terms in the model as Aesthetics in the aes() argument, naming them the same as in your
#' data: \code{aes(Group1 = Group1, Group2 = Group2, Covariate = Covariate)}
#'
#' @inheritParams ggplot2::stat_smooth
#' @param fitted_model a model object (ex from lm or lmer) compatible with the `predict` function
#' @param se display confidence interval around smooth? (TRUE by default, see level to control)
#' @param SEs width of confidence interval band (in se's)
#' @examples
#' library(ggplot2)
#' lm_mpg = lm(hwy ~ displ,mpg)
#' ggplot(mpg,aes(x=displ,y=hwy)) + geom_point() +
#'     stat_plotlm(fitted_model = lm_mpg,aes(displ = displ),se=T)
#'
#' # add faceting
#' lm_drv = lm(hwy ~ poly(displ,2)*drv,mpg)
#' ggplot(mpg,aes(x=displ,y=hwy)) + geom_point() + facet_wrap(~drv,scales = 'free') +
#'     stat_plotlm(fitted_model = lm_drv,aes(displ = displ,drv=drv),se=T,fullrange=T,n=20)
#'
#' # add multiple colors in each facet. Note that the group aesthetic is necessary for stat_plotlm
#' lm_trans = lm(hwy ~ poly(displ,2)*drv + trans,mpg)
#' ggplot(mpg,aes(x=displ,y=hwy)) + geom_point(aes(color = drv)) + facet_wrap(~trans,scales = 'free') +
#'    stat_plotlm(fitted_model = lm_trans,
#'                aes(group = drv,color = drv,displ = displ,drv=drv,trans=trans),
#'                se=T,fullrange=T,n=20)
#' @export
stat_plotlm <- function(mapping = NULL, data = NULL, geom = "smooth",
                        position = "identity", show.legend = NA,
                        inherit.aes = TRUE, fitted_model = NULL, se=TRUE, fullrange = FALSE,n = 100,SEs = 2,
                        ...) {
  ggplot2::layer(
    stat = StatPlotlm2, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(fitted_model = fitted_model, se=se,n=n,fullrange = fullrange,SEs = SEs,...)
  )
}
