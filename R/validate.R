is_chr_nomiss <- function(x) {
  is.character(x) && (! any(is.na(x)))
}

is_bool <- function(x) {
  (is.logical(x)
    && (length(x) == 1L)
    && (! is.na(x))
  )
}
