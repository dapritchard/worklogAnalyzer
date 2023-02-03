aggregate_by_period <- function(worklogs,
                                config, # TODO: find a way to extract this from the worklogs when collecting into a df
                                tags_mapping,
                                start_time,
                                period,
                                directions,
                                default = NA_character_) {
  worklogs_df <- as_tibble(worklogs)
  maybe_tags <- maybe_translate_tags(worklogs_df, config, tags_mapping, default)
  if (maybe_tags$status == "failure") {
    msg <- c(
      "Not all worklogs entry could be mapped to a tags category\n",
      maybe_tags$message
    )
    stop(msg, call. = FALSE)
  }
  worklogs_df$.mapped_tags <- maybe_tags$mapped_tags
  partitions_collection <- partition_by_intervals(
    worklogs_df,
    start_time,
    period,
    directions,
    config
  )
  tag_categories <- `if`(
    is.na(NA_character_),
    names(tags_mapping),
    c(names(tags_mapping), default)
  )
  results <- set_names(
    map(partitions_collection$partition_df, summarize_period, config, tag_categories),
    partitions_collection$before_time
  )
  as_tibble(c(categories = list(tag_categories), results))
}


summarize_period <- function(worklogs_df,
                             config,
                             tag_categories) {
  sum_category <- function(tag) {
    sum(durations[worklogs_df$.mapped_tags == tag])
  }
  format_duration <- function(duration) {
    duration_dbl <- as.double(duration)
    sprintf(
      "%s:%02d",
      duration_dbl %% 86400 %/% 3600,
      round(duration_dbl %% 3600 / 60)
    )
  }
  durations <- worklogs_df[[config@labels@duration]]
  summed_categories <- map_dbl(tag_categories, sum_category)
  map_chr(summed_categories, format_duration)
}

maybe_translate_tags <- function(worklogs_df, config, tags_mapping, default) {
  stopifnot(
    is.list(tags_mapping),
    map_lgl(tags_mapping, is_chr_nomiss),
    is_chr_nomiss(names(tags_mapping)),
    length(unique(names(tags_mapping))) == length(tags_mapping)
  )
  tags_list <- worklogs_df[[config@labels@tags]]
  tags_mapping_df <- create_tags_mapping_df(tags_mapping)
  mapped_tags <- map_chr(tags_list, match_entry_tags, tags_mapping_df, default)
  no_match_index <- which(is.na(mapped_tags))
  if (length(no_match_index) >= 1L) {
    error_rows <- worklogs_df[no_match_index, ]
    msg <- sprintf(
      "[%s]--[%s] %s\n",
      error_rows[[config@labels@start]],
      error_rows[[config@labels@end]],
      error_rows[[config@labels@description]]
    )
    out <- list(
      status  = "failure",
      message = msg
    )
    return(out)
  }
  list(
    status      = "success",
    mapped_tags = mapped_tags
  )
}

# Try every mapping, one at a time, returning the first mapping that contains a
# match. If none are found return the `default` mapping
match_entry_tags <- function(entry_tags, tags_mapping_df, default) {
  for (i in seq_len(nrow(tags_mapping_df))) {
    if (any(entry_tags %in% tags_mapping_df$from[[i]])) {
      return(tags_mapping_df$to[i])
    }
  }
  default
}

create_tags_mapping_df <- function(tags_mapping) {
  set_names(
    enframe(tags_mapping),
    c("to", "from")
  )
}

# extract_tags <- function(matches) {
#   extract_element <- function(matches_df) {
#     `if`(
#       nrow(matches_df) == 1L,
#       matches_df$to,
#       default
#     )
#   }
#   default <- "<no match>"
#   map(matches, extract_element)
# }

# calc_tags_df_list <- function(tags, tags_mapping) {
#   tags_mapping_df <- list_to_dict(tags_mapping)
#   map(tags, ~ tags_mapping_df[tags_mapping_df$from %in% .x, ])
# }

# list_to_dict <- function(tags_mapping_list) {
#   to_dict <- function(from, to) {
#     structure(
#       .Data = rep(to, length(from)),
#       names = from
#     )
#   }
#   tags_mapping_list <- imap(tags_mapping, to_dict)
#   tags_mapping <- do.call(c, tags_mapping_list)
# }

# list_to_df <- function(tags_mapping_list) {
#   to_df <- function(from, to) {
#     tibble(
#       from = from,
#       to   = to
#     )
#   }
#   tags_mapping_list <- imap(tags_mapping_list, to_df)
#   bind_rows(tags_mapping_list)
# }
