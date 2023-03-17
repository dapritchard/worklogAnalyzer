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

create_node_from_leaf <- function(wkls) {
  stopifnot(is(wkls, "worklogs_leaf"))
  new(
    Class       = "worklogs_node",
    children    = set_names(list(wkls), wkls@name),
    fold_status = "unfolded",
    prototype   = wkls@worklogs[numeric(0L), ]
  )
}
