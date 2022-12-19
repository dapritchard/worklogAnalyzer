# TODO: check inputs
# TODO: check conflicts in `...` (e.g. `file`, `colClasses`)
# TODO: `row.names` is in output object, should this be dropped?
# TODO: optional `timezone`?
# TODO: add duration
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
  colClasses <- structure(
    .Data = rep("NULL", ncol(header_row)),
    names = c(t(header_row))
  )
  colClasses[col_labels] <- "character"
  wkls_orig <- read.csv(
    file       = file,
    colClasses = colClasses,
    ...        = ...
  )
  wkls <- as_tibble(wkls_orig)
  if ("start" %in% names(col_formats)) {
    wkls[["start"]] <- strptime(
      x      = wkls[["start"]],
      format = col_formats['start'],
      tz     = col_formats['timezone']
    )
  }
  if ("end" %in% names(col_formats)) {
    wkls[["end"]] <- strptime(
      x      = wkls[["end"]],
      format = col_formats['end'],
      tz     = col_formats['timezone']
    )
  }
  structure(
    .Data      = wkls,
    class      = c("worklogs", class(wkls)),
    col_labels = col_labels
  )
}
