parse_wkls <- function(wkls_df) {
  stopifnot(is.data.frame(wkls_df))
}

read_wkls <- function(file, col_labels, col_formats, ...) {
  stopifnot(
    is.character(file),
    is.character(col_labels),
    is.character(col_formats)
  )
  header_row <- read.csv(
    file       = file,
    header     = FALSE,
    colClasses = "character",
    nrows      = 1L
  )
  wkls <- read.csv(
    file       = file,
    colClasses = colClasses,
    ...        = ...
  )
  # dplyr::mutate(
  #   .data = wkls,
  #   start = strptime(x = start, format = "%Y-%m-%d %H:%M", tz = "US/Eastern"),
  #   end   = strptime(x = end, format = "%Y-%m-%d %H:%M", tz = "US/Eastern"),
  #   tags  = strsplit(tags, ":")
  # )
}
