#' pair.generator: Generate pairs for pairwise comparisons projects
#'
#' The pair.generator package is designed to generate pairs for comparisons of
#' educational perfromances.  Use this package to generate pairs and allocate
#' them to judges, then use a judging interface and an estimation proceedure.
#'
#' Performances are paired in a way which guarantees that no performance is left
#' out of the set of pairs.  Each performance is included in the set within a
#' specified range.  The score difference between performances in a pair can be
#' restriced.  Ability scales can be created (after estimation of performance
#' locations - not included here) using standard pairs, which can be created
#' using the \code{pairs_generate} function.  Or, scales can be extended using a
#' 'core versus non-core' approach, using the \code{pairs_generate_cnc} function
#' to generate pairs.  Diagnostic functions are available for inspecting the
#' properties of generated pairs.  See the vignettes for more information and
#' examples.
#'
#' @docType package
#' @name pair.generator
NULL
