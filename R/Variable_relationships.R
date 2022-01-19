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
is_nested = function(outer_term,inner_term,data = NULL) {
  if(is(outer_term,'formula')) {
    if(!is.data.frame(inner_term)) stop('must provide a data.frame')
    formula = outer_term
    data = inner_term
    if(!all(all.vars(formula) %in% colnames(data))) {
      vars = all.vars(formula)
      vars = vars[!vars %in% colnames(data)]
      stop(sprintf('Variable(s): %s are not columns of your data',paste(vars,collapse=', ')))
    }
    outer_term = formula[[2]]
    inner_term = formula[[3]]
    # should change this code to test subspaces.
    outer = interaction(lapply(all.vars(outer_term),function(x) data[[x]]),drop=T)
    inner = interaction(lapply(all.vars(inner_term),function(x) data[[x]]),drop=T)
    all(colSums(table(outer,inner) > 0L) == 1L)
  } else{
    if(is.null(data)) stop('must provide a data.frame')
    if(!outer_term %in% colnames(data)) stop(sprintf('%s not a column of you data.table',outer_term))
    if(!inner_term %in% colnames(data)) stop(sprintf('%s not a column of you data.table',inner_term))
    all(colSums(table(data[[outer_term]],data[[inner_term]]) > 0L) == 1L)
  }
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
  if(is(term1,'formula')) {
    if(!is.data.frame(term2)) stop('must provide a data.frame')
    formula = term1
    data = term2
    term_to_char = function(x) paste(all.vars(x),collapse=':')
    is_nested(formula,data) & is_nested(formula(sprintf('%s~%s',term_to_char(formula[[3]]),term_to_char(formula[[2]]))),data)
  } else{
    if(!term1 %in% colnames(data)) stop(sprintf('%s not a column of you data.table',term1))
    if(!term2 %in% colnames(data)) stop(sprintf('%s not a column of you data.table',term2))
    is_nested(term1,term2,data) & is_nested(term2,term1,data)
  }
}


#' Checks pairs of Variables for a crossed relationship
#'
#' Given two variable names and a data.table, checks if the \code{term1} is crossed with the \code{term2}.
#' Two variables are at least partially crossed if at least some levels of \code{term1} are paired
#' with multiple levels of \code{term2} and vice verse. We consider \emph{partially crossed} a sufficient
#' condition for crossed relationships here. In some cases, we care about fully crossed.
#'
#' @param formula, term1 ~ term2
#' @param data data.frame with columns (minimally) \code{term1} and \code{term2}
#' @param only_fully_crossed = FALSE If TRUE, then will return TRUE only if the variables are fully crossed,
#' meaning that every level of \code{term1} is paired with every level of \code{term2} and vice versa.
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is_crossed = function(formula, data,only_full_crossed = F) {
  if(!is.data.frame(data)) stop('must provide a data.frame')
  if(!all(all.vars(formula) %in% colnames(data))) {
    vars = all.vars(formula)
    vars = vars[!vars %in% colnames(data)]
    stop(sprintf('Variable(s): %s are not columns of your data',paste(vars,collapse=', ')))
  }
  term1_name = formula[[2]]
  term2_name = formula[[3]]
  term1 = interaction(lapply(all.vars(term1_name),function(x) data[[x]]),drop=T)
  term2 = interaction(lapply(all.vars(term2_name),function(x) data[[x]]),drop=T)

  t = table(term1,term2)
  names(dimnames(t)) = c(term1_name,term2_name)
  if(only_full_crossed) {
    print(all(t > 0L))
  } else{
    print(all(rowSums(t>0L)>1) & all(colSums(t>0L) > 1))
  }
  invisible(t)
}
