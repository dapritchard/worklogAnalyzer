mk_worklogs_node <- function(wkls, split_dfs, config) {
  new_wkls <- mk_worklogs_impl_node(wkls, split_dfs, "", config)
  is_no_prototype_node <- map_lgl(
    .x     = new_wkls@children,
    .f     = is,
    class2 = "no_prototype_node"
  )
  len_new_wkls <- length(new_wkls@children)
  if ((len_new_wkls == 0L) || any(is_no_prototype_node)) {
    stop("Can't use the `worklogs` constructor without including any data frames")
  }
  new_wkls
}

#' @rdname worklogs
#' @export
setMethod("worklogs",
  signature  = "list",
  definition = mk_worklogs_node
)

mk_worklogs_leafs <- function(wkls, split_dfs, config) {
  stopifnot(
    is.data.frame(wkls),
    is_bool(split_dfs),
    is(config, "worklogs_config")
  )
  if (split_dfs) {
    new_wkls <- mk_worklogs_impl_leafs_split_yes(wkls, config)
  }
  else {
    # If there are no rows then we can't infer a name to use for the leaf so we
    # give up
    if (nrow(wkls) == 0L) {
      msg <- paste0(
        "Cannot instantiate a `worklogs_leaf` object with 0 rows using ",
        "`worklogs` when the `split_dfs` argument is `FALSE`"
      )
      stop(msg, call. = FALSE)
    }
    # Since `split_dfs` is `FALSE`, `wkls` is required to have exactly 1 task
    # type, but we'll delegate that check to `mk_worklogs_impl_leafs_split_no`
    task_name <- wkls[[config@labels@description]][1L]
    wrapped_wkls <- set_names(list(wkls), task_name)
    new_wkls <- mk_worklogs_impl_node(wrapped_wkls, split_dfs, "", config)
  }
  new_wkls
}

#' @rdname worklogs
#' @export
setMethod("worklogs",
  signature  = "data.frame",
  definition = mk_worklogs_leafs
)

setGeneric("worklogs_impl",
  def = function(wkls, split_dfs, name, config)
    standardGeneric("worklogs_impl"),
  signature = "wkls"
)

# fixup_prototypes_do <- function(wkls, prototype) {
#   is_no_prototype_node <- map_lgl(raw_worklogs_node_orig, is, "no_prototype_node")
# }

# add_prototype <- function(wkls, prototype) {
#   is_no_prototype_node <- function(x) {
#     is(x, "no_prototype_node")
#   }
#   stopifnot(is(wkls, "no_prototype_node"))
#   children <-
#   new("worklogs_node", wkls@children, wkls@fold_status, prototype)

#   modify_if(wkls, is_no_prototype_node, add_prototype, prototype)
# }

mk_worklogs_impl_node <- function(wkls, split_dfs, name, config) {
  extract_maybe_prototype <- function(wkls_list) {
    is_no_prototype_node <- map_lgl(wkls_list, is, "no_prototype_node")
    if (any(! is_no_prototype_node)) {
      child_worklog <- wkls_list[! is_no_prototype_node][[1L]]
      maybe_prototype <- extract_prototype(child_worklog)
    }
    else {
      maybe_prototype <- NULL
    }
    maybe_prototype
  }
  # try_adding_prototypes <- function(wkls) {
  #   is_no_prototype_node <- map_lgl(raw_worklogs_node_orig, is, "no_prototype_node")
  #   if (any(is_no_prototype_node) && (! all(is_no_prototype_node))) {
  #     child_worklog <- raw_worklogs_node[! is_no_prototype_node][[1L]]
  #     prototype <- extract_prototype(child_worklog)
  #     new_wkls <- add_prototype(wkls, prototype)
  #   }
  #   else {
  #     new_wkls <- wkls
  #   }
  #   new_wkls
  # }
  # add_prototypes <- function() {
  #   is_no_prototype_node <- function(x) {
  #     is(x, "no_prototype_node")
  #   }
  #   child_worklog <- raw_worklogs_node[! is_no_prototype_node][[1L]]
  #   prototype <- extract_prototype(child_worklog)
  #   modify_if(raw_worklogs_node, is_no_prototype_node, add_prototype, prototype)
  # }
  stopifnot(
    is.list(wkls),
    is_chr_nomiss_norepeat(names(wkls)),
    is_bool(split_dfs),
    is_string(name),
    is(config, "worklogs_config")
  )
  children <- imap(
    .x        = wkls,
    .f        = worklogs_impl,
    split_dfs = split_dfs,
    config    = config
  )
  maybe_prototype <- extract_maybe_prototype(children)
  if (is.null(maybe_prototype)) {
    new_wkls <- new(
      Class       = "no_prototype_node",
      children    = children,
      fold_status = "unfolded"
    )
  }
  else {
    updated_children <- map(children, add_prototype, maybe_prototype)
    new_wkls <- new(
      Class       = "worklogs_node",
      children    = updated_children,
      fold_status = "unfolded",
      prototype   = maybe_prototype
    )
  }
  new_wkls
  # children <- `if`(
  #   is.null(maybe_prototype),
  #   new(
  #     Class       = "no_prototype_node",
  #     children    = children,
  #     fold_status = "unfolded"
  #   ),
  #   new(
  #     Class       = "no_prototype_node",
  #     children    = add_prototype(children, prototype),
  #     fold_status = "unfolded",
  #     prototype   = maybe_prototype
  #   )
  # )
}

setMethod("worklogs_impl",
  signature  = "list",
  definition = mk_worklogs_impl_node
)

mk_worklogs_impl_leafs <- function(wkls, split_dfs, name, config) {
  `if`(
    split_dfs,
    mk_worklogs_impl_leafs_split_yes(wkls, config),
    mk_worklogs_impl_leafs_split_no(wkls, name, config)
  )
}

mk_worklogs_impl_leafs_split_yes <- function(wkls, config) {
  stopifnot(is.data.frame(wkls), is(config, "worklogs_config"))
  raw_leafs <- split(wkls, wkls[[config@labels@description]])  # TODO: helper function for wkls[[config@labels@description]]
  worklogs_leafs <- imap(
    .x = raw_leafs,
    .f = ~ new("worklogs_leaf", worklogs = .x, name = .y, config = config)
  )
  new("worklogs_node", children = worklogs_leafs, fold_status = "unfolded")
}

mk_worklogs_impl_leafs_split_no <- function(wkls, name, config) {
  stopifnot(
    is.data.frame(wkls),
    length(unique(wkls[[config@labels@description]])) <= 1L  # TODO: helper function for wkls[[config@labels@description]]
  )
  worklogs_leaf <- new("worklogs_leaf", worklogs = wkls, name = name, config = config)
}

setMethod("worklogs_impl",
  signature  = "data.frame",
  definition = mk_worklogs_impl_leafs
)
