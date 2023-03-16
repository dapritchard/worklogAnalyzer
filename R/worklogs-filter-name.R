setGeneric("process_worklog_by_match",
  def       = function(wkls, is_match, recurse_thunk) standardGeneric("process_worklog_by_match"),
  signature = "wkls"
)

process_worklog_by_match_node <- function(wkls, is_match, recurse_thunk) {
  `if`(is_match, wkls, recurse_thunk())
}

setMethod("process_worklog_by_match",
  signature  = "worklogs_node",
  definition = process_worklog_by_match_node
)

process_worklog_by_match_leaf <- function(wkls, is_match, recurse_thunk) {
  `if`(is_match, wkls, NULL)
}

setMethod("process_worklog_by_match",
  signature  = "worklogs_leaf",
  definition = process_worklog_by_match_leaf
)

#' Filter worklogs by name
#'
#' Filter the elements of a worklog tree by name. For `filter_name_keep`, all
#' worklog tree elements of the appropriate type with names that are matched by
#' any of the elements of `pattern` are kept, minus any worklog tree elements
#' with names that are matched by any of the elements of `exclude`. Subtrees
#' that don't contain any worklogs after filtering can optionally be pruned from
#' the tree.
#'
#' All worklog tree elements (i.e. nodes and leafs) have an associated name,
#' with the exception of the top level. `filter_name_keep` provides the ability
#' to filter elements of the worklog tree based on their names and types.
#'
#' `filter_name_keep` performs filtering by walking the worklog tree. For a
#' given subtree in the tree, children nodes and children leafs are treated
#' differentially according to the input to the `type` argument, as described in
#' the following section.
#'
#' @section Filtering algorithm:
#'
#' * If `type` is `"nodes"`:
#'
#'   * For a node: when a node name is matched then include the entire subtree
#'     rooted by the node; otherwise recurse into the node and perform filtering
#'     on the node children.
#'   * For a leaf: the leaf is discarded
#'
#' * If `type` is `"both"`:
#
#'   * For a node: when a node name is matched then include the entire subtree
#'     rooted by the node; otherwise recurse into the node and perform filtering
#'     on the node children.
#'   * For a leaf: keep the leaf if the name is a match, otherwise the leaf is
#'     discarded.
#
#' * If `type` is `"leaf"`:
#'
#'   * For a node: recurse into the node and perform filtering on the node
#'     children.
#'   * For a leaf:  keep the leaf if the name is a match, otherwise the leaf is
#'     discarded.
#'
#' @section Regular expression pattern matching:
#'
#' When performing filtering, for a given input and tree node element the names
#' of the node children are matched against the regular expressions provided for
#' the `pattern` and `exclude` arguments.
#'
#' Regular expression pattern matching works the same for the input to `pattern`
#' as for `exclude` as follows. For each worklog child name of the relevant type
#' (as determined by the input to the `type` argument), if any of the regular
#' expressions provided as elements of the argument input character vector match
#' the worklog child name then that worklog child is marked as being included or
#' excluded, as appropriate.
#'
#' Regular expression pattern matching is performed by the [base::grepl]
#' function. You can use `...` to provide arguments that will be passed on to
#' `grepl`.
#'
#' @param wkls A `worklogs` object.
#' @param pattern A character vector of regular expressions that will be used to
#'   match against the `worklogs_node` and `worklog_leaf` names.
#' @param type One of either `"leafs"`, `"nodes"`, or `"both"` specifying which
#'   types of elements in the worklogs tree to perform matching upon.
#' @param exclude A character vector of regular expressions that will be used to
#'   match against the `worklogs_node` and `worklog_leaf` names.
#' @param prune_empty Either `TRUE` or `FALSE` specifying whether to prune any
#'   subtrees that don't contain worklogs in them after filtering.
#' @param ... Arguments that are passed on to [base::grepl] to be used for
#'   regular expression matching with `pattern` and `exclude`.
#' @name filter_name
NULL

#' @export
#' @rdname filter_name
setGeneric("filter_name_keep",
  def       = function(wkls, pattern, type, exclude, prune_empty, ...) standardGeneric("filter_name_keep"),
  signature = "wkls"
)

# TODO: make into a method
check_worklogs_empty <- function(wkls) {
  length(wkls@children) == 0L
}

filter_name_keep_node <- function(wkls,
                                  pattern = character(0L),
                                  type = "leafs",
                                  exclude = character(0L),
                                  prune_empty = TRUE,
                                  ...) {
  calc_match_lgl <- function(children) {
    nodes_lgl <- map_lgl(children, is, class2 = "worklogs_node")
    if (type == "leafs") {
      type_mask <- (! nodes_lgl)
    } else if (type == "nodes") {
      type_mask <- nodes_lgl
    } else {
      type_mask <- rep(TRUE, length(children))
    }
    init_false <- rep(FALSE, length(children))
    include_lgls <- map(pattern, grepl, x = names(children), ...)
    include_lgl <- reduce(include_lgls, `|`, .init = init_false)
    exclude_lgls <- map(exclude, grepl, x = names(children), ...)
    exclude_lgl <- reduce(exclude_lgls, `|`, .init = init_false)
    type_mask & include_lgl & (! exclude_lgl)
  }
  process_child <- function(child, is_match) {
    # TODO: this call will fail if called when `child` is a `worklogs_leaf`. We
    # are currently assuming that the thunk will only be used on nodes, but
    # maybe it would be better to make different thunks depending on node type?
    recurse_thunk <- function() {
      new_child <- filter_name_keep(child, pattern, type, exclude, prune_empty, ...)
      `if`(prune_empty && check_worklogs_empty(new_child), NULL, new_child)
    }
    process_worklog_by_match(child, is_match, recurse_thunk)
  }
  stopifnot(
    is(wkls, "worklogs"),
    is_chr_nomiss(pattern),
    check_arg_type(type),
    is_chr_nomiss(exclude),
    is_bool(prune_empty)
  )
  children <- wkls@children
  keep_lgl <- calc_match_lgl(children)
  maybe_children <- map2(children, keep_lgl, process_child)
  wkls@children <- compact(maybe_children)
  wkls
}

#' @rdname filter_name
#' @export
setMethod("filter_name_keep",
  signature  = "worklogs_node",
  definition = filter_name_keep_node
)

# TODO: move this to a better place
check_arg_type <- function(x) {
  (is_string(x)
    & (x %in% c("leafs", "nodes", "both"))
  )
}
