maybe_translate_tags <- function(worklogs_df, tags_mapping, default = NULL, config) {
  stopifnot(
    is.list(tags_mapping),
    map_lgl(tags_mapping, is_chr_nomiss),
    is_chr_nomiss(tags_mapping),
    length(unique(names(tags_mapping))) == length(tags)
  )
  tags <- worklogs_df[[config@labels@tags]]
  matches <- calc_tags_df_list(tags, tags_mapping)
  n_matches <- map_int(matches, nrow)
  if (any(n_matches >= 2L)) {
    error_rows <- worklogs_df[n_matches >= 2L, ]
    msg <- sprintf(
      "%s -- %s: %s\n",
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
  extract_tags(matches)
}

maybe_translate_entry_tags <- function(entry_tags, tags_mapping_df, default) {
  # Try every mapping, one at a time, selecting the first mapping that contains
  # a match. If none are found use the `default` mapping
  for i in seq_along(entry_tags) {
    # Return the mapping if one is found
    if (any(entry_tags %in% tags_mapping_df$from[[i]])) {
      return(tags_mapping_df$to)
    }
  }
  # If we've made it here then no match has been found, so return the fallback
  # value
  default
}

extract_tags <- function(matches) {
  extract_element <- function(matches_df) {
    `if`(
      nrow(matches_df) == 1L,
      matches_df$to,
      default
    )
  }
  default <- "<no match>"
  map(matches, extract_element)
}

calc_tags_df_list <- function(tags, tags_mapping) {
  tags_mapping_df <- list_to_dict(tags_mapping)
  map(tags, ~ tags_mapping_df[tags_mapping_df$from %in% .x, ])
}

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

list_to_df <- function(tags_mapping_list) {
  to_df <- function(from, to) {
    tibble(
      from = from,
      to   = to
    )
  }
  tags_mapping_list <- imap(tags_mapping_list, to_df)
  bind_rows(tags_mapping_list)
}
