#' Example data set
#'
#' Use with \code{pairs_generate}. All media are "core". Used in projects where
#' a new scale is to be created.
#'
#' @docType data
#'
#' @usage data(data_standard)
#'
#' @format A data frame with 3 variables. Column headers are \code{media},
#'   \code{core} & \code{score}.
#' \describe{
#'   \item{media}{The label of the performance
#'   or media item.  Often a file name.}
#'   \item{core}{1 for core, 0 for non-core.
#'   Core is for making a new scale, non-core is for locating a performance on
#'   an existing scale.}
#'   \item{score}{Examples of student scores out of 100.}
#' }
"data_standard"


#' Example data for core versus non-core pair generation
#'
#' Use with \code{pairs_generate_cnc}.
#' Used in projects where non-core items are to be located on a predetermined
#' scale of the core items.
#'
#' @docType data
#'
#' @usage data(data_cnc)
#'
#' @format A data frame with 3 variables. Column headers are \code{media, core, score}
#' \describe{
#'   \item{media}{The label of the performance or media
#'   item.}
#'   \item{core}{1 for core, 0 for non-core.  Core is for making a new scale,
#'   non-core is for locating a performance on an existing scale.}
#'   \item{score}{Examples of student scores out of 100.}
#'   }
"data_cnc"

