#' Add an extra header row above the current header
#' @export
htmlTable_add_header_above <- function(kable_input, header = NULL) {
  if (is.null(header)) return(kable_input)
  kable_xml <- read_xml(as.character(kable_input), options = c("COMPACT"))
  kable_xml_thead <- xml_child(kable_xml, "thead")


  x <- read_xml("<parent><child>1</child><child>2<child>3</child></child></parent>")
  children <- xml_children(x)
  t1 <- children[[1]]
  t2 <- children[[2]]
  t3 <- xml_children(children[[2]])[[1]]


}
