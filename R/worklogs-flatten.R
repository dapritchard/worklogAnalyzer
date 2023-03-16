setGeneric("collect_worklogs",
  def       = function(wkls, parent_task) standardGeneric("collect_worklogs"),
  signature = "wkls"
)

collect_worklogs_node <- function(wkls, parent_task) {
  update_task_name <- function(wkls_df) {
    description_label <- attr(wkls_df, "description_label", TRUE)
    wkls_df[[description_label]] <- parent_task
    wkls_df
  }
  stopifnot(is(wkls, "worklogs_node"))
  collected_children_dfs_list <- imap(wkls@children, collect_worklogs)
  collected_children_dfs <- list_flatten(collected_children_dfs_list)
  `if`(
    fold_status(wkls) == "folded",
    map(collected_children_dfs, update_task_name),
    collected_children_dfs
  )
}

setMethod("collect_worklogs",
  signature  = "worklogs_node",
  definition = collect_worklogs_node
)

collect_worklogs_leaf <- function(wkls, parent_task) {
  stopifnot(is(wkls, "worklogs_leaf"))
  structure(
    .Data             = wkls@worklogs,
    description_label = wkls@config@labels@description
  )
}

setMethod("collect_worklogs",
  signature  = "worklogs_leaf",
  definition = collect_worklogs_leaf
)

#' Collect a Worklogs Node Into a Tibble
#'
#' Collects all of the worklogs in a worklogs tree into a tibble.
#'
#' @param x A `worklogs_node`.
#' @param ... Arguments that are passed on to `as_tibble`.
#'
#' @export
as_tibble.worklogs_node <- function(x, ...) {
  collected_worklogs_list <- collect_worklogs_node(x, "<top level>")
  collected_worklogs <- bind_rows(collected_worklogs_list)
  attr(collected_worklogs, "description_label") <- NULL
  as_tibble(collected_worklogs, ...)
}

#' Collect a Worklogs Node Into a Tibble
#'
#' Extract the worklogs from a `worklogs_leaf` and apply `as_tibble` to it.
#'
#' @inheritParams as_tibble.worklogs_node
#'
#' @export
as_tibble.worklogs_leaf <- function(x, ...) {
  as_tibble(x@worklogs, ...)
}
