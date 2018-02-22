correspond <- function(data, correspondence, data.by, correspondence.from, correspondence.to, correspondence.weight) {
  correspondence <- dplyr::select_(correspondence, correspondence.from, correspondence.to, correspondence.weight)
  correspondence <- dplyr::rename_(correspondence, .dots = setNames(correspondence.from, "from"))
  correspondence <- dplyr::rename_(correspondence, .dots = setNames(correspondence.to, "to"))
  correspondence <- dplyr::rename_(correspondence, .dots = setNames(correspondence.weight, "weight"))

  merged <- dplyr::left_join(data, correspondence, by = setNames("from", data.by)) %>%
    select_(paste("-",data.by))
  for (col in colnames(merged)) {
    if (!(col %in% group_vars(merged)) &&  col != "to" && col != "weight") {
      mutate_call = lazyeval::interp(~ a * b, a = as.name(col), b = as.name("weight"))
      merged <- dplyr::mutate_(merged, .dots = setNames(list(mutate_call), col))
    }
  }

  merged <- dplyr::select(merged, -weight)
  merged <- dplyr::group_by(merged, to, add = TRUE)
  merged <- dplyr::summarise_all(merged, funs(sum))
  merged <- dplyr::rename_(merged,  .dots = setNames("to", correspondence.to))

  return(merged)
}
