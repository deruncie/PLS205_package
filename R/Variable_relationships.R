#' Checks pairs of Variables for a nested relationship
#'
#' Given two variable names and a data.table, checks if the \code{inner_term} is nested in the \code{outer_term},
#' meaning that each level of \code{inner_term} is paired with at most 1 level of \code{outer_term}.
#' Each level of \code{outer_term} may be paired with multiple levels of \code{inner_term}.
#'
#' @param outer_term Term expected to be at the higher level
#' @param inner_term Term that may be nested in \code{outer_term}
#' @param data data.frame with columns (minimally) \code{outer_term} and \code{inner_term}
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is_nested = function(outer_term,inner_term,data) {
  if(!outer_term %in% colnames(data)) stop(sprintf('%s not a column of you data.table',outer_term))
  if(!inner_term %in% colnames(data)) stop(sprintf('%s not a column of you data.table',inner_term))
  all(colSums(table(data[[outer_term]],data[[inner_term]]) > 0L) == 1L)
}

#' Checks pairs of Variables for a aliased relationship
#'
#' Given two variable names and a data.table, checks if the \code{term1} is aliased with the \code{term2}.
#' Two terms are \emph{aliased} if there levels match 1:1.
#' Each level of \code{term1} is paired with exactly 1 level of \code{term2}.
#'
#' @param term1, One Variable name of interest.
#' @param term2 A second Variable name of interest.
#' @param data data.frame with columns (minimally) \code{term1} and \code{term2}
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is_aliased = function(term1, term2, data) {
  if(!term1 %in% colnames(data)) stop(sprintf('%s not a column of you data.table',term1))
  if(!term2 %in% colnames(data)) stop(sprintf('%s not a column of you data.table',term2))
  is_nested(term1,term2,data) & is_nested(term2,term1,data)
}


#' Checks pairs of Variables for a crossed relationship
#'
#' Given two variable names and a data.table, checks if the \code{term1} is crossed with the \code{term2}.
#' Two variables are at least partially crossed if at least some levels of \code{term1} are paired
#' with multiple levels of \code{term2} and vice versa. We consider \emph{partially crossed} a sufficient
#' condition for crossed relationships here. In some cases, we care about fully crossed.
#'
#' @param term1, One Variable name of interest.
#' @param term2 A second Variable name of interest.
#' @param data data.frame with columns (minimally) \code{term1} and \code{term2}
#' @param only_fully_crossed = FALSE If TRUE, then will return TRUE only if the variables are fully crossed,
#' meaning that every level of \code{term1} is paired with every level of \code{term2} and vice versa.
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is_crossed = function(term1, term2, data) {
  if(!term1 %in% colnames(data)) stop(sprintf('%s not a column of you data.table',term1))
  if(!term2 %in% colnames(data)) stop(sprintf('%s not a column of you data.table',term2))
  if(!only_full_crossed) {
    is_nested(term1,term2,data) & is_nested(term2,term1,data)
  } else{
    all(table(data[[term1]],data[[term2]])) > 0
  }
}
