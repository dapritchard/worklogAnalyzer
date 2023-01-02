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

setGeneric("fold",
  def       = function(wkls, path) standardGeneric("fold"),
  signature = "wkls"
)

fold_impl <- function(wkls, path) {
  stopifnot(is(wkls, "worklogs"))
  update_child(wkls, path, character(0L), fold_this)
}

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

setGeneric("fold_children",
  def       = function(wkls, path) standardGeneric("fold_children"),
  signature = "wkls"
)

fold_children_impl <- function(wkls, path) {
  stopifnot(is(wkls, "worklogs"))
  update_child(wkls, path, character(0L), fold_these_children)
}

setMethod("fold_children",
  signature  = "worklogs",
  definition = fold_children_impl
)

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

setGeneric("extract_worklogs",
  def       = function(wkls, path) standardGeneric("extract_worklogs"),
  signature = "wkls"
)

setMethod("extract_worklogs",
  signature  = "worklogs",
  definition = function(wkls, path) extract_worklogs_impl(wkls, path, "")
)

setGeneric("collect_worklogs",
  def       = function(wkls, parent_task) standardGeneric("collect_worklogs"),
  signature = "wkls"
)

collect_worklogs_node <- function(wkls, parent_task) {
  update_task_name <- function(wkls_df) {
    stopifnot("task" %in% names(wkls_df))
    wkls_df$task <- parent_task
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
  wkls@worklogs
}

setMethod("collect_worklogs",
  signature  = "worklogs_leaf",
  definition = collect_worklogs_leaf
)

as_tibble.worklogs_node <- function(x, ...) {
  collected_worklogs_list <- collect_worklogs_node(x, "<top level>")
  collected_worklogs <- bind_rows(collected_worklogs_list)
  as_tibble(collected_worklogs, ...)
}

as_tibble.worklogs_leaf <- function(x, ...) {
  as_tibble(x@worklogs, ...)
}


# effort -----------------------------------------------------------------------

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

format_effort_node <- function(effort, padding, depth, total_effort, config) {
  mk_top_levels <- function(children, padding) {
    mk_folded_info <- function(effort) {
      `if`(
        fold_status(effort) == "folded",
        sprintf("(+%d) ", effort@n_descendents),
        ""
      )
    }
    mk_effort_percents <- function(children) {
      sum_efforts <- map_dbl(children, `@`, "sum_effort")
      proportion <- sum_efforts / total_effort
      percent <- as.integer(round(100 * proportion))
      # spaces <- strrep(" ", 6L * depth)
      # case_when(
      #   sum_efforts == 0  ~ sprintf("%s  0%%", spaces),
      #   proportion < 0.01 ~ sprintf("%s <1%%", spaces),
      #   percent < 10L     ~ sprintf("%s  %d%%", spaces, percent),
      #   proportion < 0.99 ~ sprintf("%s %d%%", spaces, percent),
      #   TRUE              ~ sprintf("%s>99%%", spaces)
      # )
      spaces <- strrep(" ", 5L * depth)
      case_when(
        percent <= 9L ~ sprintf("%s %d%%", spaces, percent),
        TRUE          ~ sprintf("%s%d%%", spaces, percent)
      )
    }
    n <- length(children)
    glyphs <- `if`(
      n == 0L,
      character(0L),
      c(rep("├── ", n - 1L), "└── ")
    )
    folded_info_orig <- map_chr(children, mk_folded_info)
    max_folded_info <- max(nchar(folded_info_orig))
    padding_width <- max_folded_info - nchar(folded_info_orig)
    folded_info_padding <- strrep(" ", padding_width)
    folded_info <- case_when(
      folded_info_orig == "" ~ "",
      TRUE                   ~ paste0(folded_info_padding, folded_info_orig)
    )
    tasks <- sprintf("%s%s%s%s", padding, glyphs, folded_info, names(children))
    efforts <- mk_effort_percents(children)
    map2(tasks, efforts, ~ list(task = .x, effort = .y))
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
  wkls_children <- `if`(
    config$show_all,
    effort@children,
    keep(effort@children, ~ .x@sum_effort > 0)
  )
  top_levels <- mk_top_levels(wkls_children, padding)
  next_padding <- mk_next_padding(wkls_children, padding)
  formatted_children <- map2(
    .x           = wkls_children,
    .y           = next_padding,
    .f           = format_effort,
    depth        = depth + 1L,
    total_effort = total_effort,
    config       = config
  )
  combined_sections <- map2(top_levels, formatted_children, ~ c(list(.x), .y))
  flatten(combined_sections)
  # # map2(top_levels, formatted_children, c)
  # combined_sections <- map2(top_levels, formatted_children, c)
  # flatten_chr(combined_sections)
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

effort_summary <- function(wkls, show_all = FALSE) {
  config <- list(show_all = show_all)
  effort <- effort_collection(wkls)
  stopifnot(effort@sum_effort > 0)
  tree_components <- format_effort_node(effort, "", 0L, effort@sum_effort, config)
  tasks <- map_chr(tree_components, chuck, "task")
  efforts <- map_chr(tree_components, chuck, "effort")
  task_widths <- nchar(tasks)
  stopifnot(length(task_widths) >= 1L)
  max_task_width = max(task_widths)
  task_padding <- strrep(" ", max_task_width + 2L - task_widths)
  effort_summary <- paste0(tasks, task_padding, efforts)
  effort_column_header_components <- c("Effort proportion", "-----------------")
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
  wkls@worklogs <- f(wkls@worklogs)
  wkls
}

setMethod("update_worklogs",
  signature  = "worklogs_leaf",
  definition = update_worklogs_leaf
)

setGeneric("filter_time_before",
  def       = function(wkls, datetime) standardGeneric("filter_time_before"),
  signature = "wkls"
)

filter_time_before_impl <- function(wkls, datetime) {
  filter_fcn <- function(wkls_df) {
    wkls_df[wkls_df$start < datetime, ]
  }
  stopifnot(
    is(wkls, "worklogs"),
    inherits(datetime, "POSIXct")
  )
  update_worklogs(wkls, filter_fcn)
}

setMethod("filter_time_before",
  signature  = "worklogs",
  definition = filter_time_before_impl
)

setGeneric("filter_time_after",
  def       = function(wkls, datetime) standardGeneric("filter_time_after"),
  signature = "wkls"
)

filter_time_after_impl <- function(wkls, datetime) {
  filter_fcn <- function(wkls_df) {
    wkls_df[wkls_df$start >= datetime, ]
  }
  stopifnot(
    is(wkls, "worklogs"),
    inherits(datetime, "POSIXct")
  )
  update_worklogs(wkls, filter_fcn)
}

setMethod("filter_time_after",
  signature  = "worklogs",
  definition = filter_time_after_impl
)

setGeneric("filter_time_between",
  def = function(wkls, before_datetime, after_datetime)
    standardGeneric("filter_time_between"),
  signature = "wkls"
)

filter_time_between_impl <- function(wkls, before_datetime, after_datetime) {
  filter_fcn <- function(wkls_df) {
    is_after_start <- before_datetime <= wkls_df$start
    is_before_end <- wkls_df$start < after_datetime
    wkls_df[is_after_start & is_before_end, ]
  }
  stopifnot(
    is(wkls, "worklogs"),
    inherits(before_datetime, "POSIXct"),
    inherits(after_datetime, "POSIXct")
  )
  update_worklogs(wkls, filter_fcn)
}

setMethod("filter_time_between",
  signature  = "worklogs",
  definition = filter_time_between_impl
)

setGeneric("filter_this_week",
  def       = function(wkls) standardGeneric("filter_this_week"),
  signature = "wkls"
)

calc_this_week_start <- function(datetime) {
  n_day <- wday(datetime)
  datetime - days(n_day - 1)
}

# calc_this_week_end <- function(datetime) {
#   n_day <- wday(datetime)
#   datetime + days(8 - n_day)
# }

filter_this_week_impl <- function(wkls) {
  filter_fcn <- function(wkls_df) {
    wkls_df[wkls_df$start >= this_week_start, ]
  }
  stopifnot(is(wkls, "worklogs"))
  this_week_start <- calc_this_week_start(today())
  update_worklogs(wkls, filter_fcn)
}

setMethod("filter_this_week",
  signature  = "worklogs",
  definition = filter_this_week_impl
)

setGeneric("filter_last_week",
  def       = function(wkls) standardGeneric("filter_last_week"),
  signature = "wkls"
)

calc_last_week_start <- function(datetime) {
  n_day <- wday(datetime)
  datetime - days(n_day + 6)
}

filter_last_week_impl <- function(wkls) {
  filter_fcn <- function(wkls_df) {
    is_after_start <- last_week_start <= wkls_df$start
    is_before_end <- wkls_df$start < this_week_start
    wkls_df[is_after_start & is_before_end, ]
  }
  stopifnot(is(wkls, "worklogs"))
  today_datetime <- today()
  last_week_start <- calc_last_week_start(today_datetime)
  this_week_start <- calc_this_week_start(today_datetime)
  update_worklogs(wkls, filter_fcn)
}

setMethod("filter_last_week",
  signature  = "worklogs",
  definition = filter_last_week_impl
)
