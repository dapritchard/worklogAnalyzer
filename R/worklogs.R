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
  raw_worklogs_node <- map(x, worklogs)
  new("worklogs_node", children = raw_worklogs_node)
}

setMethod("worklogs",
  signature  = "list",
  definition = mk_worklogs_node
)

mk_worklogs_leafs <- function(wkls, split_dfs) {
  add_class_info <- function(raw_leaf) {
    structure(
      .Data = raw_leaf,
      class = c("wkls", class(wkls))
    )
  }
  stopifnot(is.data.frame(wkls))
  raw_tibble <- as_tibble(wkls)
  raw_leafs <- split(raw_tibble, raw_tibble$task)
  worklogs_leafs_list <- map(raw_leafs, ~ new("worklogs_leaf", worklogs = .x))
  new("worklogs_node", children = worklogs_leafs_list)
}

setMethod("worklogs",
  signature  = "data.frame",
  definition = mk_worklogs_leafs
)


# cloj <- select(out$personal$`programming exercises`$`exercism: Clojure`, -parents)
# class(cloj) <- class(cloj)[-1L]
# sml <- select(out$personal$`programming exercises`$`exercism: Standard ML`, -parents)
# class(sml) <- class(sml)[-1L]
# z <- list(cloj = cloj, sml = sml)

# worklogs <- function(x) {
#   stopifnot(is.list(x))
#   if (is_worklogs(x)) {
#     return(x)
#   }
#   else if (is.data.frame(x)) {
#     return(mk_worklogs_leafs(x))
#   }
#   else {
#     return(mk_worklogs_node(x))
#   }
# }



# assert_raw_worklogs_node <- function(x) {
#   stopifnot(
#     is.list(x),
#     ! is.null(names(x))
#   )
# }

assert_raw_worklogs_leaf <- function(x) {
  stopifnot(
    is.data.frame(x)
  )
}

assert_string <- function(x) {
  stopifnot(
    is.character(x),
    length(x) == 1L,
    ! is.na(x)
  )
}

is_worklogs <- function(x) {
  is_worklogs_node(x) || is_worklogs_leaf(x)
}

is_worklogs_node <- function(x) {
  inherits(x, "worklogs_node")
}

is_worklogs_leaf <- function(x) {
  inherits(x, "worklogs_leaf")
}
