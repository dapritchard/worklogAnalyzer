setGeneric("format_worklogs",
  def       = function(wkls, padding) standardGeneric("format_worklogs"),
  signature = "wkls"
)

format_worklogs_node <- function(wkls, padding) {
  `if`(
    fold_status(wkls) == "unfolded",
    format_worklogs_node_unfolded(wkls, padding),
    format_worklogs_node_folded(wkls, padding)
  )
}

format_worklogs_node_unfolded <- function(wkls, padding) {
  mk_top_levels <- function(children, padding) {
    mk_folded_info <- function(wkls) {
      `if`(
        fold_status(wkls) == "folded",
        sprintf("(+%d) ", count_descendents(wkls)),
        ""
      )
    }
    n <- length(children)
    glyphs <- `if`(
      n == 0L,
      character(0L),
      # A unicode version of c(rep("|-- ", n - 1L), "`-- ")
      c(rep("\u251C\u2500\u2500 ", n - 1L), "\u2514\u2500\u2500 ")
    )
    folded_info <- map_chr(children, mk_folded_info)
    sprintf("%s%s%s%s", padding, glyphs, folded_info, names(children))
  }
  mk_next_padding <- function(children, padding) {
    n <- length(children)
    new_padding <- `if`(
      n == 0L,
      character(0L),
      # A unicode version of c(rep("|  ", n - 1L), "   ")
      c(rep("\u2502  ", n - 1L), "   ")
    )
    sprintf("%s%s", padding, new_padding)
  }
  wkls_children <- wkls@children
  top_levels <- mk_top_levels(wkls_children, padding)
  next_padding <- mk_next_padding(wkls_children, padding)
  formatted_children <- map2(wkls_children, next_padding, format_worklogs)
  combined_sections <- map2(top_levels, formatted_children, c)
  flatten_chr(combined_sections)
}

format_worklogs_node_folded <- function(wkls, padding) {
  character(0L)
}

setMethod("format_worklogs",
  signature  = "worklogs_node",
  definition = format_worklogs_node
)

format_worklogs_leaf <- function(wkls, padding) {
  character(0L)
}

setMethod("format_worklogs",
  signature  = "worklogs_leaf",
  definition = format_worklogs_leaf
)

#' Display a `worklogs` Object
#'
#' @param object A `worklogs` object.
#'
#' @importMethodsFrom methods show
#' @export
setMethod("show",
  signature  = "worklogs",
  definition = print_worklogs
)
