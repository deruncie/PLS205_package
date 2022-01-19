# library(lme4)
# d = expand.grid(A=factor(1:3),B=factor(1:4))
# d$C = paste(d$B,1:3,sep=':')
# d = rbind(d,d)
# d$X = 1:nrow(d)
# d$y = rnorm(nrow(d))
#
# f = y~A+B+A:B+(1|C)+X
#
# str(get_all_vars(f,d))
# remove_b
#
# # print_values(x) {
# #   if(is.numeric(x)) {
# #     if(length(x) >5) {
# #       paste(c(signif(x[1:5,digits=4]),'...'),collapse=' ')
# #     } else{
# #       paste(signif(x,digits=4]),collapse=' ')
# #     }
# #   } else{
# #     if(is.factor(x)) {
# #       l = levels(x)
# #       if(length(l))
# #     }
# #   }
# # }
#
# d1 = d

#' check_data Checks a data.frame for all the terms in a model
#'
#' Prepares a report list \code{str(data)} that displays the number of levels of each factor
#' including factor combinations
#'
#' @param object Either a formula, e.g. \code{y~A+B+(1|C)}
#' or the output of a call to \code{lm()} or \code{lmer()}
#' @param d If \code{object} is a formula, provide the data.frame.
#' If \code{object} is the output of a model statement, leave NULL.
#'
#' @return nothing. Just prints the complete data structure.
#' @export
#'
#' @examples
check_data = function(object,d = NULL) {
  f=formula(object)
  if(is.null(d)) {
    d = model.frame(object)
  }
  # First, use model.frame to check that the terms exist
  mf = model.frame(lme4::subbars(f),d)
  # now extract each term
  expanded_d = list()
  all_terms = terms(lme4::subbars(f))
  response = as.character(all_terms[[2]])
  expanded_d[[response]] = d[[response]]

  for(t in all.vars(all_terms)) {
    if(!is.numeric(d[[t]])) d[[t]] = droplevels(as.factor(d[[t]]))
  }

  for(t in labels(all_terms)) {
    if(grepl(':',t,fixed=T)[[1]]) {
      expanded_d[[t]] = do.call(interaction,list(lapply(strsplit(t,':')[[1]],function(v) d[[v]]),drop=TRUE))
    } else{
      expanded_d[[t]] = d[[t]]
    }
  }
  expanded_d = data.frame(expanded_d,check.names = F)
  # all(model.matrix(subbars(f),d) == model.matrix(subbars(f),expanded_d))
  print(f)
  str(expanded_d)
}
