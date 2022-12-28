setClass("worklogs_node",
  slots     = c(children = "list", fold_status = "character"),
  prototype = list(
    children    = structure(list(), names = character(0L)),
    fold_status = NA_character_
  )
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
  else if (! (object@fold_status %in% c("folded", "unfolded"))) {
    return("@fold_status of %s is invalid", object@fold_status)
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
  new("worklogs_node", children = raw_worklogs_node, fold_status = "unfolded")
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
  new("worklogs_node", children = worklogs_leafs_list, fold_status = "unfolded")
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

setGeneric("count_descendents",
  def = function(wkls) standardGeneric("count_descendents")
)

count_descendents_node <- function(wkls) {
  children <- wkls@children
  sum(map_int(children, count_descendents))
}

setMethod("count_descendents",
  signature  = "worklogs_node",
  definition = count_descendents_node
)

count_descendents_leaf <- function(wkls) {
  1L
}

setMethod("count_descendents",
  signature  = "worklogs_leaf",
  definition = count_descendents_leaf
)

print_worklogs <- function(object) {
  wkls_str <- format_worklogs(object, "")
  cat(".\n")
  cat(wkls_str, sep = "\n")
}

setGeneric("fold_status",
  def       = function(wkls) standardGeneric("fold_status"),
  signature = "wkls"
)

setMethod("fold_status",
  signature  = "worklogs_node",
  definition = function(wkls) wkls@fold_status
)

setMethod("fold_status",
  signature  = "worklogs_leaf",
  definition = function(wkls) "unfolded"
)

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
      c(rep("├── ", n - 1L), "└── ")
    )
    folded_info <- map_chr(children, mk_folded_info)
    sprintf("%s%s%s%s", padding, glyphs, folded_info, names(children))
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

setMethod("show",
  signature  = "worklogs",
  definition = print_worklogs
)
