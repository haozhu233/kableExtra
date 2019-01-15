#' Read HTML kable as XML
#'
#' @description This function will read kable as a xml file
#'
#' @param x kable or kableExtra object
#'
#' @export
kable_as_xml <- function(x) {
  read_kable_as_xml(x)
}

#' Convert XML back to kable
#'
#' @description Convert XML back to kable
#'
#' @param x XML table object
#'
#' @export
xml_as_kable <- function(x) {
  as_kable_xml(x)
}
