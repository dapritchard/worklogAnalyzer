description <- c(
  "task A",
  "task B",
  "task C"
)

start <- lubridate::parse_date_time(
  x      = c("2023-01-10 14:10", "2023-01-10 15:19",  "2023-01-11 08:59"),
  orders = "ymd H:M"
)

end <- lubridate:::parse_date_time(
  x      = c("2023-01-10 14:40", "2023-01-10 15:50",  "2023-01-11 10:02"),
  orders = "ymd H:M"
)

worklogs_df <- tibble(
  description = description,
  start       = start,
  end         = end
)

tags <- list(
  c("X", "Y"),
  c("Y", "Z"),
  c("Q", "R", "S")
)

tags_mapping <- list(
  "1" = "Q",
  "2" = c("Z", "S"),
  "3" = "A"# ,
  # "4" = c("X", "Y")
)

config <- worklogs_config(
  description_label = "description",
  start_label       = "start",
  end_label         = "end",
  duration_label    = "duration",
  tags_label        = "tags"
)

worklogs_df <- tibble(
  description = description,
  start       = start,
  end         = end,
  duration    = end - start,
  tags        = tags
)

# maybe_translate_tags(worklogs_df, config, tags_mapping)
