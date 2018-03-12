#' Convert data from one geography to another
#'
#' @param data The data you wish to convery
#' @param correspondence The correspondence
#' @param data.by The column name that identifies the area IDs in your data
#' @param correspondence.from The column name that identifies the from areas in the correspondence
#' @param correspondence.to The column name that identifies the to areas in the correspondence
#' @param correspondence.weight The column name that identifies the weights in the correspondence
#'
#' @export
correspond <- function(data, correspondence, data.by, correspondence.from, correspondence.to, correspondence.weight) {
  correspondence <- dplyr::select_(correspondence, correspondence.from, correspondence.to, correspondence.weight)
  correspondence <- dplyr::rename_(correspondence, .dots = stats::setNames(correspondence.from, "from"))
  correspondence <- dplyr::rename_(correspondence, .dots = stats::setNames(correspondence.to, "to"))
  correspondence <- dplyr::rename_(correspondence, .dots = stats::setNames(correspondence.weight, "weight"))

  merged <- dplyr::left_join(data, correspondence, by = stats::setNames("from", data.by))
  merged <- dplyr::select_(merged, paste("-",data.by))

  for (col in colnames(merged)) {
    if (!(col %in% dplyr::group_vars(merged)) &&  col != "to" && col != "weight") {
      mutate_call = lazyeval::interp(~ a * b, a = as.name(col), b = as.name("weight"))
      merged <- dplyr::mutate_(merged, .dots = stats::setNames(list(mutate_call), col))
    }
  }

  merged <- dplyr::select_(merged, "-weight")
  merged <- dplyr::group_by_(merged, "to", add = TRUE)
  merged <- dplyr::summarise_all(merged, dplyr::funs(sum))
  merged <- dplyr::rename_(merged,  .dots = stats::setNames("to", correspondence.to))

  return(merged)
}
