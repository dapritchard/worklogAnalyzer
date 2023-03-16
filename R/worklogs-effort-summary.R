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
  normalize_duration <- function(duration) {
    units_type <- units(duration)
    if (units_type == "secs") {
      multiplier <- 1
    } else if (units_type == "mins") {
      multiplier <- 60
    }
    else {
      stop("internal error: no formating for '", units_type, "' units")
    }
    sum(as.double(duration)) * multiplier
  }
  stopifnot(is(wkls, "worklogs_leaf"))
  list(
    sum_effort    = normalize_duration(wkls@worklogs$duration),
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
    fold_status   = "unfolded",
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

mk_effort_chr <- function(sum_efforts) {
  sum_efforts_minutes <- as.integer(sum_efforts / 60)
  hours <- sum_efforts_minutes %/% 60L
  minutes <- sum_efforts_minutes %% 60L
  minutes_chr <- if_else(
    minutes <= 9L,
    sprintf("0%d", minutes),
    sprintf("%d", minutes)
  )
  sprintf("%d:%s", hours, minutes_chr)
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
      effort_chr <- mk_effort_chr(sum_efforts)
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
  mk_effort_total <- function() {
    if (config$effort_style == "effort_and_percent") {
      effort_total <- sprintf("%s (100%%)", mk_effort_chr(effort@sum_effort))
    }
    else if (config$effort_style == "effort") {
      effort_total <- mk_effort_chr(effort@sum_effort)
    }
    else if (config$effort_style == "percent") {
      effort_total <- "100%"
    }
    else {
      stop(
        "Internal error: invalid value of config$effort_style: ",
        config$effort_style
      )
    }
    effort_total
  }
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
  effort_total <- sprintf("Total: %s", mk_effort_total())
  effort_column_header_components <- c("Effort proportion", strrep("\u2500", 17L))
  effort_column_header_padding <- c(
    strrep(" ", max(nchar(effort_summary)) - 17L - nchar(effort_total)),
    strrep(" ", max(nchar(effort_summary)) - 18L)
  )
  effort_column_header <- paste0(
    c(effort_total, "."),
    effort_column_header_padding,
    effort_column_header_components
  )
  cat(effort_column_header, sep = "\n")
  cat(effort_summary, sep = "\n")
}

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
