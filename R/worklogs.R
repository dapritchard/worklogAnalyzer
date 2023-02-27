# `worklogs` configuration classes ---------------------------------------------

#' Configuration Classes
#'
#' S4 classes used in specifying worklogs configuration information.
#'
#' @slot description A string specifying the column name of the worklog
#'   description.
#' @slot start A string specifying the column name of the worklog start times.
#' @slot end A string specifying the column name of the worklog end times.
#' @slot duration A string specifying the column name of the worklog durations.
#' @slot tags A string specifying the column name of the worklog tags.
#'
#' @name classes_worklogs_config
NULL

#' @rdname classes_worklogs_config
#' @export
setClass("config_labels",
  slots = c(
    description = "character",
    start       = "character",
    end         = "character",
    duration    = "character",
    tags        = "character"
 ),
 prototype = list(
    description = NA_character_,
    start       = NA_character_,
    end         = NA_character_,
    duration    = NA_character_,
    tags        = NA_character_
  )
)

# #' @rdname configuration_classes
# #' @export
# setClass("config_formats",
#   slots = c(
#     start    = "character",
#     end      = "character",
#     duration = "character"
#  ),
#  prototype = list(
#     start    = NA_character_,
#     end      = NA_character_,
#     duration = NA_character_
#   )
# )

#' @slot labels A `config_labels` object.
#' @rdname classes_worklogs_config
#' @export
setClass("worklogs_config",
  slots     = c(labels = "config_labels"),
  prototype = list(labels = new("config_labels"))
)

#' Create a `worklogs_config` Object
#'
#' This is the intended mechanism for creating `worklogs_config` objects.
#'
#' @param description_label A string specifying the column name of the worklog
#'   descriptions.
#' @param start_label Either a string specifying the column name of the worklog
#'   start times, or `NA_character_` if there is no such column.
#' @param end_label Either a string specifying the column name of the worklog
#'   end times, or `NA_character_` if there is no such column.
#' @param duration_label Either a string specifying the column name of the
#'   worklog duration entries, or `NA_character_` if there is no such column.
#' @param tags_label Either a string specifying the column name of the worklog
#'   tags, or `NA_character_` if there is no such column.
#'
#' @return A `worklogs_config` object.
#'
#' @export
worklogs_config <- function(description_label,
                            start_label = NA_character_,
                            end_label = NA_character_,
                            duration_label = NA_character_,
                            tags_label = NA_character_) {
  # TODO: check the following:
  # - at least 2 out of 3 of start_label, end_label, and end_format
  stopifnot(
    is_string(description_label),
    is_maybe_string(start_label),
    is_maybe_string(end_label),
    is_maybe_string(duration_label),
    is_maybe_string(tags_label)
  )
  new("worklogs_config",
    labels = new("config_labels",
      description = description_label,
      start       = start_label,
      end         = end_label,
      duration    = duration_label,
      tags        = tags_label
    )# ,
    # formats = new("config_formats",
    #   start    = start_format,
    #   end      = end_format,
    #   duration = duration_format
    # ),
    # timezone = timezone
  )
}


# `worklog` class defintions ---------------------------------------------------

#' Configuration Classes
#'
#' S4 classes used for representing worklogs.
#'
#' @slot children A named list of `worklogs` objects.
#' @slot fold_status Either `"unfolded"` or `"folded"`.
#' @slot prototype A data frame with 0 rows used to specify the form of all
#'   children worklog entries.
#'
#' @name classes_worklogs
NULL

#' @rdname classes_worklogs
#' @export
setClass("worklogs_node",
  slots     = c(
    children    = "list",
    fold_status = "character",
    prototype   = "data.frame"
  ),
  prototype = list(
    children    = structure(list(), names = character(0L)),
    fold_status = "unfolded"
    prototype   = data.frame()
  )
)

#' @slot worklogs A data frame of worklog entries.
#' @slot name A string providing the name of the worklogs.
#' @slot config A `worklogs_config` object.
#' @rdname classes_worklogs_config
#' @export
setClass("worklogs_leaf",
  slots     = c(
    worklogs = "data.frame",
    name     = "character",
    config   = "worklogs_config"
  ),
  prototype = list(
    worklogs = data.frame(),
    name     = "",
    config   = new("worklogs_config")
  )
)

#' @rdname classes_worklogs_config
#' @export
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

#' Create a `worklogs` Object
#'
#' This is one of several available functions that can be used to create
#' `worklogs` objects.
#'
#' @param wkls Either a data frame of worklog entries, or a named list
#' @param split_dfs Either `TRUE` or `FALSE`.
#' @param config A `worklogs_config` object.
#'
#' @return A `worklogs` object.
#'
#' @name worklogs
#' @export
setGeneric("worklogs",
  def       = function(wkls, split_dfs, config) standardGeneric("worklogs"),
  signature = "wkls"
)

mk_worklogs_node <- function(wkls, split_dfs, config) {
  mk_worklogs_impl_node(wkls, split_dfs, "", config)
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

mk_worklogs_impl_node <- function(wkls, split_dfs, name, config) {
  stopifnot(
    is.list(wkls),
    is_chr_nomiss_norepeat(names(wkls)),
    is_bool(split_dfs),
    is_string(name),
    is(config, "worklogs_config")
  )
  raw_worklogs_node <- imap(
    .x        = wkls,
    .f        = worklogs_impl,
    split_dfs = split_dfs,
    config    = config
  )
  new("worklogs_node", children = raw_worklogs_node, fold_status = "unfolded")
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


# `worklogs` `show` method -----------------------------------------------------

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


# Updating `worklog` graph elements --------------------------------------------

setGeneric("update_child",
  def       = function(wkls, path, parents, f) standardGeneric("update_child"),
  signature = "wkls"
)

update_child_node <- function(wkls, path, parents, f) {

  # Update the appropriate child of `wkls`
  update_child <- function() {

    # Extract children
    children <- wkls@children

    # Update `path` and `parents` information for upcoming call
    stopifnot(length(path) >= 1L)
    child_name <- path[1L]
    new_path <- path[-1L]
    new_parents <- c(parents, child_name)

    # Update the appropriate child
    child <- chuck(children, child_name)
    new_child <- update_child_node(child, new_path, new_parents, f)
    stopifnot(is(new_child, "worklogs"))
    validObject(new_child)

    # Update the `children` slot and return `wkls`
    pluck(children, child_name) <- new_child
    wkls@children <- children
    validObject(wkls)
    wkls
  }

  `if`(
    length(path) == 0L,
    f(wkls),
    update_child()
  )
}

setMethod("update_child",
  signature  = "worklogs_node",
  definition = update_child_node
)

update_child_leaf <- function(wkls, path, parents, f) {
  if (length(path) >= 1L) {
    msg <- sprintf(
      "%s\n%s%s\n%s",
      "Can't get the child of a leaf node with the following parents:",
      sprintf("    %s\n", path),
      "The following children were asked for: ",
      sprintf("    %s\n", path)
    )
    stop(msg)
  }
  f(wkls)
}

setMethod("update_child",
  signature  = "worklogs_leaf",
  definition = update_child_leaf
)

setGeneric("fold_this",
  def       = function(wkls) standardGeneric("fold_this"),
  signature = "wkls"
)

fold_this_node <- function(wkls) {
  stopifnot(is(wkls, "worklogs_node"))
  wkls@fold_status <- "folded"
  validObject(wkls)
  wkls
}

setMethod("fold_this",
  signature  = "worklogs_node",
  definition = fold_this_node
)

fold_this_leaf <- function(wkls) {
  stopifnot(is(wkls, "worklogs_leaf"))
  wkls
}

setMethod("fold_this",
  signature  = "worklogs_leaf",
  definition = fold_this_leaf
)

#' Fold Worklogs Below a Given Location
#'
#' Nodes in a worklogs tree can be flagged as being _folded_, which has the
#' effect of letting functions that operate on worklogs know to consider all
#' descendents of that node to belong to that node.
#'
#' @param wkls A `worklogs` object.
#' @param path A character vector such that each element in vector corresponds
#'   to the name of an element in the `worklogs` tree.
#'
#' @return A `worklogs` object.
#' @export
setGeneric("fold",
  # TODO: consider calling this `fold_element` for better symmetry with `fold_children`
  def       = function(wkls, path) standardGeneric("fold"),
  signature = "wkls"
)

fold_impl <- function(wkls, path) {
  stopifnot(is(wkls, "worklogs"))
  update_child(wkls, path, character(0L), fold_this)
}

#' @rdname fold
#' @export
setMethod("fold",
  signature  = "worklogs",
  definition = fold_impl
)

setGeneric("fold_these_children",
  def       = function(wkls) standardGeneric("fold_these_children"),
  signature = "wkls"
)

fold_these_children_node <- function(wkls) {
  stopifnot(is(wkls, "worklogs_node"))
  wkls@children <- map(wkls@children, fold_this)
  validObject(wkls)
  wkls
}

setMethod("fold_these_children",
  signature  = "worklogs_node",
  definition = fold_these_children_node
)

fold_these_children_leaf <- function(wkls) {
  stopifnot(is(wkls, "worklogs_leaf"))
  wkls
}
setMethod("fold_these_children",
  signature  = "worklogs_leaf",
  definition = fold_these_children_leaf
)

#' @rdname fold
#' @export
setGeneric("fold_children",
  def       = function(wkls, path) standardGeneric("fold_children"),
  signature = "wkls"
)

fold_children_impl <- function(wkls, path) {
  stopifnot(is(wkls, "worklogs"))
  update_child(wkls, path, character(0L), fold_these_children)
}

#' @rdname fold
#' @export
setMethod("fold_children",
  signature  = "worklogs",
  definition = fold_children_impl
)


# Extract `worklog` graph elements -------------------------------------------------------

setGeneric("extract_worklogs_impl",
  def       = function(wkls, path, parents) standardGeneric("extract_worklogs_impl"),
  signature = "wkls"
)

extract_worklogs_impl_node <- function(wkls, path, parents) {

  # Update the appropriate child of `wkls`
  extract_worklogs_impl <- function() {

    # Extract children
    children <- wkls@children

    # Update `path` and `parents` information for upcoming call
    stopifnot(length(path) >= 1L)
    child_name <- path[1L]
    new_path <- path[-1L]
    new_parents <- c(parents, child_name)

    # Extract the appropriate child
    child <- chuck(children, child_name)
    extract_worklogs_impl_node(child, new_path, new_parents)
  }

  `if`(
    length(path) == 0L,
    wkls,
    extract_worklogs_impl()
  )
}

setMethod("extract_worklogs_impl",
  signature  = "worklogs_node",
  definition = extract_worklogs_impl_node
)

extract_worklogs_impl_leaf <- function(wkls, path, parents) {
  if (length(path) >= 1L) {
    msg <- sprintf(
      "%s\n%s%s\n%s",
      "Can't get the child of a leaf node with the following parents:",
      sprintf("    %s\n", path),
      "The following children were asked for: ",
      sprintf("    %s\n", path)
    )
    stop(msg)
  }
  wkls
}

setMethod("extract_worklogs_impl",
  signature  = "worklogs_leaf",
  definition = extract_worklogs_impl_leaf
)

#' Extract a Subtree From a Worklogs Tree
#'
#' @param wkls A `worklogs` object.
#' @param path A character vector such that each element in vector corresponds
#'
#' @export
setGeneric("extract_worklogs",
  def       = function(wkls, path) standardGeneric("extract_worklogs"),
  signature = "wkls"
)

#' @rdname extract_worklogs
#' @export
setMethod("extract_worklogs",
  signature  = "worklogs",
  definition = function(wkls, path) extract_worklogs_impl(wkls, path, "")
)


# Collect worklogs (e.g. reassemble into a data frame) -------------------------

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


# Effort summary ---------------------------------------------------------------

# TODO: create validObject for the various classes

setGeneric("extract_effort",
  def       = function(wkls) standardGeneric("extract_effort"),
  signature = "wkls"
)

extract_effort_node <- function(wkls) {
  stopifnot(is(wkls, "worklogs_node"))
  children_effort <- map(wkls@children, extract_effort)
  list(
    sum_effort    = sum(map_dbl(children_effort, chuck, "sum_effort")),
    n_descendents = sum(map_int(children_effort, chuck, "n_descendents"))
  )
}

setMethod("extract_effort",
  signature  = "worklogs_node",
  definition = extract_effort_node
)

extract_effort_leaf <- function(wkls) {
  stopifnot(is(wkls, "worklogs_leaf"))
  list(
    sum_effort    = sum(as.numeric(wkls@worklogs$duration)),
    n_descendents = 1L
  )
}

setMethod("extract_effort",
  signature  = "worklogs_leaf",
  definition = extract_effort_leaf
)

setClass("effort_node",
  slots = c(
    children      = "list",
    sum_effort    = "numeric",
    n_descendents = "integer"
  ),
  prototype = list(
    children      = structure(list(), names = character(0L)),
    sum_effort    = NA_real_,
    n_descendents = NA_integer_
  )
)

setClass("effort_leaf",
  slots = c(
    fold_status   = "character",
    sum_effort    = "numeric",
    n_descendents = "integer"
  ),
  prototype = list(
    fold_status   = NA_character_,
    sum_effort    = NA_real_,
    n_descendents = NA_integer_
  )
)

setClassUnion("effort", c("effort_node", "effort_leaf"))

setMethod("fold_status",
  signature  = "effort_node",
  definition = function(wkls) "unfolded"
)

setMethod("fold_status",
  signature  = "effort_leaf",
  definition = function(wkls) wkls@fold_status
)

# setGeneric("effort_fold_status",
#   def       = function(effort) standardGeneric("effort_fold_status"),
#   signature = "effort"
# )

# setMethod("effort_fold_status",
#   signature  = "effort_node",
#   definition = function(effort) effort@fold_status
# )

# setMethod("effort_fold_status",
#   signature  = "effort_leaf",
#   definition = function(effort) "unfolded"
# )

setGeneric("effort_collection",
  def       = function(wkls) standardGeneric("effort_collection"),
  signature = "wkls"
)

effort_collection_node <- function(wkls) {
  mk_effort_collection_node <- function(wkls) {
    children_effort_collection <- map(wkls@children, effort_collection)
    sum_effort <- sum(map_dbl(children_effort_collection, `@`, "sum_effort"))
    n_descendents <- sum(map_int(children_effort_collection, `@`, "n_descendents"))
    new(
      Class         = "effort_node",
      children      = children_effort_collection,
      sum_effort    = sum_effort,
      n_descendents = n_descendents
    )
  }
  mk_effort_collection_leaf <- function(wkls) {
    total <- extract_effort(wkls)
    new(
      Class         = "effort_leaf",
      fold_status   = "folded",
      sum_effort    = chuck(total, "sum_effort"),
      n_descendents = chuck(total, "n_descendents")
    )
  }
  stopifnot(is(wkls, "worklogs_node"))
  `if`(
    fold_status(wkls) == "unfolded",
    mk_effort_collection_node(wkls),
    mk_effort_collection_leaf(wkls)
  )
}

setMethod("effort_collection",
  signature  = "worklogs_node",
  definition = effort_collection_node
)

effort_collection_leaf <- function(wkls) {
  stopifnot(is(wkls, "worklogs_leaf"))
  # TODO: same code as for `mk_effort_collection_leaf`
  total <- extract_effort(wkls)
  new(
    Class         = "effort_leaf",
    fold_status   = "unfolded",
    sum_effort    = chuck(total, "sum_effort"),
    n_descendents = chuck(total, "n_descendents")
  )
}

setMethod("effort_collection",
  signature  = "worklogs_leaf",
  definition = effort_collection_leaf
)

setGeneric("format_effort",
  def = function(effort, padding, depth, total_effort, config)
    standardGeneric("format_effort"),
  signature = "effort"
)

# format_effort_node <- function(effort, padding, depth, total_effort, config) {
#   `if`(
#     fold_status(effort) == "unfolded",
#     format_effort_node(effort, padding, depth, total_effort, config),
#     format_effort_node_folded(effort, padding, depth, total_effort, config)
#   )
# }

pad_left <- function(s) {
  stopifnot(is.character(s))
  if (length(s) == 0L) {
    return(s)
  }
  n_chars <- nchar(s)
  padding <- strrep(" ", max(n_chars) - n_chars)
  paste0(padding, s)
}

pad_right <- function(s) {
  stopifnot(is.character(s))
  if (length(s) == 0L) {
    return(s)
  }
  n_chars <- nchar(s)
  padding <- strrep(" ", max(n_chars) - n_chars)
  paste0(s, padding)
}

pad_entries_only <- function(s) {
  stopifnot(is.character(s))
  if (length(s) == 0L) {
    return(s)
  }
  n_chars <- nchar(s)
  padding <- case_when(
    s == "" ~ "",
    TRUE    ~ strrep(" ", max(n_chars) - n_chars)
  )
  paste0(padding, s)
}

format_effort_node <- function(effort, padding, depth, total_effort, config) {
  mk_top_levels <- function(children, padding) {
    mk_folded_info <- function(effort) {
      `if`(
        fold_status(effort) == "folded",
        sprintf("(+%d) ", effort@n_descendents),
        ""
      )
    }
    mk_efforts_info <- function(children) {
      sum_efforts <- map_dbl(children, `@`, "sum_effort")
      sum_efforts_minutes <- as.integer(sum_efforts / 60)
      hours <- sum_efforts_minutes %/% 60L
      minutes <- sum_efforts_minutes %% 60L
      minutes_chr <- if_else(
        minutes <= 9L,
        sprintf("0%d", minutes),
        sprintf("%d", minutes)
      )
      effort_chr <- sprintf("%d:%s", hours, minutes_chr)
      # effort_chr <- pad_left(effort_orig_chr)
      proportion <- sum_efforts / total_effort
      percent_int <- as.integer(round(100 * proportion))
      # percent_chr <- case_when(
      #   percent <= 9L ~ sprintf("%s %d%%", pad_effort, percent_int),
      #   TRUE          ~ sprintf("%s%d%%", pad_effort, percent_int)
      # )
      percent_chr <- sprintf("%d%%", percent_int)
      tibble(
        effort  = effort_chr,
        percent = percent_chr
      )
    }
    # mk_efforts <- function(efforts_info) {
    #   if (config$effort_style == "effort_and_percent") {
    #     efforts <- paste0(
    #       pad_effort,
    #       pad_left(efforts_info$effort),
    #       " ",
    #       pad_left(sprintf("(%s)", efforts_info$percent))
    #     )
    #   }
    #   else if (config$effort_style == "effort") {
    #     efforts <- sprintf("%s%s", pad_effort, pad_left(efforts_info$effort))
    #   }
    #   else if (config$effort_style == "percent") {
    #     efforts <- sprintf("%s%s", pad_effort, pad_left(efforts_info$percent))
    #   }
    #   else {
    #     stop("Internal error: invalid value of config$effort_style: ", config$effort_style)
    #   }
    #   efforts
    # }
    n <- length(children)
    glyphs <- `if`(
      n == 0L,
      character(0L),
      # A unicode version of c(rep("|-- ", n - 1L), "`-- ")
      c(rep("\u251C\u2500\u2500 ", n - 1L), "\u2514\u2500\u2500 ")
    )
    folded_info <- pad_entries_only(map_chr(children, mk_folded_info))
    tasks <- sprintf("%s%s%s%s", padding, glyphs, folded_info, names(children))
    efforts_info <- mk_efforts_info(children)
    # efforts <- mk_efforts(efforts_info)
    transpose(tibble(task = tasks, efforts_info, depth = depth))
    # map2(tasks, transpose(efforts_info), ~ list(task = .x, effort = .y))
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
  # mk_next_pad_effort <- function(top_levels) {
  #   next_pad_effort <- `if`(
  #     length(top_levels) == 0L,
  #     "",
  #     strrep(" ", nchar(top_levels[[1L]]$effort))
  #   )
  #   sprintf("  %s", next_pad_effort)
  # }
  wkls_children <- `if`(
    config$show_all,
    effort@children,
    keep(effort@children, ~ .x@sum_effort > 0)
  )
  top_levels <- mk_top_levels(wkls_children, padding)
  next_inputs <- list(
    effort     = wkls_children,
    padding    = mk_next_padding(wkls_children, padding),
    depth      = depth + 1L
    # pad_effort = mk_next_pad_effort(top_levels)
  )
  formatted_children <- pmap(
    .l           = next_inputs,
    # .x           = wkls_children,
    # .y           = mk_next_padding(wkls_children, padding),
    .f           = format_effort,
    total_effort = total_effort,
    config       = config
  )
  combined_sections <- map2(top_levels, formatted_children, ~ c(list(.x), .y))
  flatten(combined_sections)
}

# format_effort_node_folded <- function(effort, padding, depth, total_effort, config) {
#   character(0L)
# }

setMethod("format_effort",
  signature  = "effort_node",
  definition = format_effort_node
)

format_effort_leaf <- function(effort, padding, depth, total_effort, config) {
  character(0L)
}

setMethod("format_effort",
  signature  = "effort_leaf",
  definition = format_effort_leaf
)

#' Display a Summary of the Efforts in the Worklogs
#'
#' @param wkls A `worklogs` object.
#' @param show_all Either `TRUE` or `FALSE` specifying whether or not to display
#'   elements of the worklog tree that didn't have any effort.
#' @param effort_style One of either "`effort`", `"percent"` or
#'   `"effort_and_percent"` specifying what information to provide in the effort
#'   summary.
#' @return Returns `NULL`.
#'
#' @export
effort_summary <- function(wkls, show_all = FALSE, effort_style = "percent") {
  config <- list(
    effort_style = effort_style,
    show_all = show_all
  )
  effort <- effort_collection(wkls)
  stopifnot(effort@sum_effort > 0)
  tree_components <- format_effort_node(effort, "", 0L, effort@sum_effort, config)
  effort_info_df <- mk_effort_info_df(tree_components)
  effort_descr_df <- mk_effort_descr_df(effort_info_df, config)
  efforts <- format_effort_tree(effort_descr_df, "", 0L)
  tasks <- pad_right(map_chr(tree_components, "task"))
  effort_summary <- sprintf("%s  %s", tasks, efforts)
  # A unicode version of c("Effort proportion", strrep("-", 17L))
  effort_column_header_components <- c("Effort proportion", strrep("\u2500", 17L))
  effort_column_header_padding <- strrep(" ", max(nchar(effort_summary)) - 18L)
  effort_column_header <- paste0(
    c(" ", "."),
    effort_column_header_padding,
    effort_column_header_components
  )
  cat(effort_column_header, sep = "\n")
  cat(effort_summary, sep = "\n")
}


# filter time routines ---------------------------------------------------------

setGeneric("update_worklogs",
  def       = function(wkls, f) standardGeneric("update_worklogs"),
  signature = "wkls"
)

update_worklogs_node <- function(wkls, f) {
  stopifnot(is(wkls, "worklogs_node"))
  wkls@children <- map(wkls@children, update_worklogs, f = f)
  wkls
}

setMethod("update_worklogs",
  signature  = "worklogs_node",
  definition = update_worklogs_node
)

update_worklogs_leaf <- function(wkls, f) {
  stopifnot(is(wkls, "worklogs_leaf"))
  f(wkls)
}

setMethod("update_worklogs",
  signature  = "worklogs_leaf",
  definition = update_worklogs_leaf
)

#' Filter worklogs by timestamp
#'
#' TODO
#'
#' @param wkls A `worklogs` object.
#' @param datetime A length-1 datetime.
#' @param before_datetime A length-1 datetime.
#' @param after_datetime A length-1 datetime.
#'
#' @name filter_time
NULL

#' @export
#' @rdname filter_time
setGeneric("filter_time_before",
  def       = function(wkls, datetime) standardGeneric("filter_time_before"),
  signature = "wkls"
)

filter_time_before_impl <- function(wkls, datetime) {
  filter_fcn <- function(wkls) {
    start_label <- wkls@config@labels@start
    wkls_df <- wkls@worklogs[[start_label]]
    new_wkls_df <- wkls_df[wkls_df[[start_label]] < datetime, ]
    new("worklogs_leaf", worklogs = new_wkls_df, config = wkls@config)
  }
  stopifnot(
    is(wkls, "worklogs"),
    inherits(datetime, "POSIXct")
  )
  update_worklogs(wkls, filter_fcn)
}

#' @rdname filter_time
#' @export
setMethod("filter_time_before",
  signature  = "worklogs",
  definition = filter_time_before_impl
)

#' @rdname filter_time
#' @export
setGeneric("filter_time_after",
  def       = function(wkls, datetime) standardGeneric("filter_time_after"),
  signature = "wkls"
)

filter_time_after_impl <- function(wkls, datetime) {
  filter_fcn <- function(wkls) {
    start_label <- wkls@config@labels@start
    wkls_df <- wkls@worklogs
    new_wkls_df <- wkls_df[wkls_df[[start_label]] >= datetime, ]
    new("worklogs_leaf", worklogs = new_wkls_df, config = wkls@config)
  }
  stopifnot(
    is(wkls, "worklogs"),
    inherits(datetime, "POSIXct")
  )
  update_worklogs(wkls, filter_fcn)
}

#' @rdname filter_time
#' @export
setMethod("filter_time_after",
  signature  = "worklogs",
  definition = filter_time_after_impl
)

#' @rdname filter_time
#' @export
setGeneric("filter_time_between",
  def = function(wkls, before_datetime, after_datetime)
    standardGeneric("filter_time_between"),
  signature = "wkls"
)

filter_time_between_impl <- function(wkls, before_datetime, after_datetime) {
  filter_fcn <- function(wkls) {
    start_label <- wkls@config@labels@start
    wkls_df <- wkls@worklogs
    is_after_start <- before_datetime <= wkls_df[[start_label]]
    is_before_end <- wkls_df[[start_label]] < after_datetime
    new_wkls_df <- wkls_df[is_after_start & is_before_end, ]
    new("worklogs_leaf", worklogs = new_wkls_df, config = wkls@config)
  }
  stopifnot(
    is(wkls, "worklogs"),
    inherits(before_datetime, "POSIXct"),
    inherits(after_datetime, "POSIXct")
  )
  update_worklogs(wkls, filter_fcn)
}

#' @rdname filter_time
#' @export
setMethod("filter_time_between",
  signature  = "worklogs",
  definition = filter_time_between_impl
)

#' @rdname filter_time
#' @export
setGeneric("filter_this_week",
  def       = function(wkls) standardGeneric("filter_this_week"),
  signature = "wkls"
)

calc_this_week_start <- function(datetime) {
  n_day <- lubridate::wday(datetime)
  datetime - lubridate::days(n_day - 1)
}

# calc_this_week_end <- function(datetime) {
#   n_day <- lubridate::wday(datetime)
#   datetime + days(8 - n_day)
# }

filter_this_week_impl <- function(wkls) {
  filter_fcn <- function(wkls) {
    start_label <- wkls@config@labels@start
    wkls_df <- wkls@worklogs
    new_wkls_df <- wkls_df[wkls_df[[start_label]] >= this_week_start, ]
    new("worklogs_leaf", worklogs = new_wkls_df, config = wkls@config)
  }
  stopifnot(is(wkls, "worklogs"))
  this_week_start <- calc_this_week_start(lubridate::today())
  update_worklogs(wkls, filter_fcn)
}

#' @rdname filter_time
#' @export
setMethod("filter_this_week",
  signature  = "worklogs",
  definition = filter_this_week_impl
)

#' @rdname filter_time
#' @export
setGeneric("filter_last_week",
  def       = function(wkls) standardGeneric("filter_last_week"),
  signature = "wkls"
)

calc_last_week_start <- function(datetime) {
  n_day <- lubridate::wday(datetime)
  datetime - lubridate::days(n_day + 6)
}

filter_last_week_impl <- function(wkls) {
  filter_fcn <- function(wkls) {
    start_label <- wkls@config@labels@start
    wkls_df <- wkls@worklogs
    is_after_start <- last_week_start <= wkls_df[[start_label]]
    is_before_end <- wkls_df[[start_label]] < this_week_start
    new_wkls_df <- wkls_df[is_after_start & is_before_end, ]
    new("worklogs_leaf", worklogs = new_wkls_df, config = wkls@config)
  }
  stopifnot(is(wkls, "worklogs"))
  today_datetime <- lubridate::today()
  last_week_start <- calc_last_week_start(today_datetime)
  this_week_start <- calc_this_week_start(today_datetime)
  update_worklogs(wkls, filter_fcn)
}

#' @rdname filter_time
#' @export
setMethod("filter_last_week",
  signature  = "worklogs",
  definition = filter_last_week_impl
)

mk_effort_info_df <- function(tree_components) {
  tibble(
    effort  = map_chr(tree_components, "effort"),
    percent = map_chr(tree_components, "percent"),
    depth   = map_int(tree_components, "depth")
  )
}


# filter worklogs by regex -----------------------------------------------------

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


# Formatting helper routines ---------------------------------------------------

mk_effort_descr_df <- function(effort_info_df, config) {
  mk_effort_descr_subset <- function(effort_info_subset_df) {
      if (config$effort_style == "effort_and_percent") {
        efforts <- paste0(
          pad_left(effort_info_subset_df$effort),
          " ",
          pad_left(sprintf("(%s)", effort_info_subset_df$percent))
        )
      }
      else if (config$effort_style == "effort") {
        efforts <- pad_left(effort_info_subset_df$effort)
      }
      else if (config$effort_style == "percent") {
        efforts <- pad_left(effort_info_subset_df$percent)
      }
      else {
        stop(
          "Internal error: invalid value of config$effort_style: ",
          config$effort_style
        )
      }
      tibble(
        effort_descr = efforts,
        depth        = effort_info_subset_df$depth
      )
  }
  description_df <- tibble(
    descr = NA_character_,
    depth = effort_info_df$depth
  )
  for (depth in unique(effort_info_df$depth)) {
    is_depth <- effort_info_df$depth %in% depth
    description_df[is_depth, ] <- mk_effort_descr_subset(effort_info_df[is_depth, ])
  }
  description_df
}

format_effort_tree <- function(effort_descr_df, padding, depth) {
  mk_next_padding <- function(children, padding, curr_effort_descr) {
    n <- length(children)
    curr_effort_padding <- `if`(
      length(curr_effort_descr) == 0L,
      character(0L),
      # strrep(" ", nchar(curr_effort_descr[1L]) + 1L)
      strrep(" ", nchar(curr_effort_descr[1L]) + 4L)
    )
    new_padding <- `if`(
      n == 0L,
      character(0L),
      # A unicode version of c(rep("|  ", n - 1L), "   ")
      c(rep("\u2502  ", n - 1L), "   ")
    )
    sprintf("%s%s%s", padding, new_padding, curr_effort_padding)
  }
  mk_glyphs <- function() {
    n <- length(curr_depth_indices)
    if (n == 0L) {
      glyphs <- character(0L)
    }
    else if (n == 1L) {
      # A unicode version of "--- "
      glyphs <- "\u2500\u2500\u2500 "
    }
    else {
      glyphs <- c(
        # A unicode version of ".-- ",
        "\u250c\u2500\u2500 ",
        # A unicode version of rep("|-- ", n - 2L),
        rep("\u251c\u2500\u2500 ", n - 2L),
        # A unicode version of "`-- "
        "\u2514\u2500\u2500 "
      )
    }
    glyphs
  }
  curr_depth_indices <- which(effort_descr_df$depth == depth)
  if (length(curr_depth_indices) == 0L) {
    return(character(0L))
  }
  subset_index_pairs_df <- tibble(
    lower = curr_depth_indices + 1L,
    upper = c(curr_depth_indices[-1L], nrow(effort_descr_df) + 1L)
  )
  # subset_index_pairs_df <- subset_index_pairs_orig_df[
  #   subset_index_pairs_orig_df$lower < subset_index_pairs_orig_df$upper,
  # ]
  subset_df_list <- pmap(
    .l = subset_index_pairs_df,
    .f = function(lower, upper) effort_descr_df[head(lower : upper, -1L), ]
  )
  curr_effort_descr <- effort_descr_df$descr[curr_depth_indices]
  glyphs <- mk_glyphs()
  top_levels <- sprintf(
    "%s%s%s",
    padding,
    glyphs,
    curr_effort_descr
  )
  next_padding <- mk_next_padding(subset_df_list, padding, curr_effort_descr)
  formatted_children <- map2(
    .x    = subset_df_list,
    .y    = next_padding,
    .f    = format_effort_tree,
    depth = depth + 1L
  )
  combined_sections <- map2(top_levels, formatted_children, c)
  flatten_chr(combined_sections)
}
