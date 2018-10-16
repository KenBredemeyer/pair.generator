#' Example data set
#'
#' Use with \code{pairs_generate}. All media are 'core'.
#' Used in projects where a new scale is to be created.
#'
#' @docType data
#'
#' @usage data(data_standard)
#'
#' @format A data frame with 3 variables. Column headers are 'media', 'core'
#'   & 'score'.
#' \describe{
#'   \item{media}{the label of the performance or media
#'   item.  Often a file name.}
#'   \item{core}{1 = core, 0 = non-core.  Core is for making a new scale,
#'   non-core is for locating on an existing scale.}
#'   \item{score}{score of the performance if available. May be missing.}
#'   }
"data_standard"


#' Example data for core versus non-core pair generation
#'
#' Use with \code{pairs_generate_cnc}.
#' Used in projects where non-core items are to be located on a pre determined
#' scale of the core items.
#'
#' @docType data
#'
#' @usage data(data_cnc)
#'
#' @format A data frame with 3 variables. Column headers are \code{media, core, score}
#' \describe{
#'   \item{media}{the label of the performance or media
#'   item.  Often a file name.}
#'   \item{core}{1 = core, 0 = non-core.  Core is for making a new scale,
#'   non-core is for locating on an existing scale.}
#'   \item{score}{score of the performance if available. May be missing.}
#'   }
"data_cnc"


#' Example student performance lables
#'
#' A mock example resembling labels of student performances used in the equating
#' of NAPLAN Writing, a large scale assessment for Australian primary and
#' secondary school students.
#'
#' @docType data
#'
#' @usage data(NAPLAN_example)
#'
#' @format Character vector.
"NAPLAN_example"
