setClass("worklogs_node",
  slots     = c(children = "list"),
  prototype = list(structure(list(), names = character(0L)))
)

setClass("worklogs_leaf",
  slots     = c(worklogs = "data.frame"),
  prototype = list(data.frame())
)

setClassUnion("worklogs", c("worklogs_node", "worklogs_leaf"))

validity_worklogs_node <- function(object) {
  children_names <- names(object@children)
  if (is.null(children_names)) {
    return("@children is required to be a named list")
  }
  else if (length(unique(children_names)) != length(children_names)) {
    return("@children names must be unique")
  }
  else if (! all(map_lgl(object@children, is, "worklogs"))) {
    return("@children must all be worklogs")
  }
  else {
    return(TRUE)
  }
}

setValidity("worklogs_node", validity_worklogs_node)

setGeneric("worklogs",
  def       = function(wkls, split_dfs) standardGeneric("worklogs"),
  signature = "wkls"
)

mk_worklogs_node <- function(wkls, split_dfs) {
  stopifnot(is.list(wkls), is_bool(split_dfs))
  raw_worklogs_node <- map(wkls, worklogs, split_dfs = split_dfs)
  new("worklogs_node", children = raw_worklogs_node)
}

setMethod("worklogs",
  signature  = "list",
  definition = mk_worklogs_node
)

mk_worklogs_leafs <- function(wkls, split_dfs) {
  `if`(
    split_dfs,
    mk_worklogs_leafs_split_yes(wkls),
    mk_worklogs_leafs_split_no(wkls)
  )
}

mk_worklogs_leafs_split_yes <- function(wkls) {
  stopifnot(is.data.frame(wkls))
  raw_tibble <- as_tibble(wkls)
  raw_leafs <- split(raw_tibble, raw_tibble$task)
  worklogs_leafs_list <- map(raw_leafs, ~ new("worklogs_leaf", worklogs = .x))
  new("worklogs_node", children = worklogs_leafs_list)
}

mk_worklogs_leafs_split_no <- function(wkls) {
  stopifnot(
    is.data.frame(wkls),
    length(unique(wkls$task)) <= 1L
  )
  raw_leaf <- as_tibble(wkls)
  worklogs_leaf <- new("worklogs_leaf", worklogs = raw_leaf)
}

setMethod("worklogs",
  signature  = "data.frame",
  definition = mk_worklogs_leafs
)



print_worklogs <- function(object) {
  wkls_str <- format_worklogs(object, "")
  cat(".\n")
  cat(wkls_str, sep = "\n")
}

setGeneric("format_worklogs",
  def       = function(wkls, padding) standardGeneric("format_worklogs"),
  signature = "wkls"
)

format_worklogs_node <- function(wkls, padding) {
  # browser()
  mk_top_levels <- function(children, padding) {
    n <- length(children)
    glyphs <- `if`(
      n == 0L,
      character(0L),
      c(rep("├── ", n - 1L), "└── ")
    )
    sprintf("%s%s%s", padding, glyphs, names(children))
  }
  mk_next_padding <- function(children, padding) {
    n <- length(children)
    new_padding <- `if`(
      n == 0L,
      character(0L),
      c(rep("│  ", n - 1L), "   ")
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

setMethod("show",
  signature  = "worklogs",
  definition = print_worklogs
)
