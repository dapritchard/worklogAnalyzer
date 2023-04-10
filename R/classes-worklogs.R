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
  slots = c(
    children    = "list",
    fold_status = "character",
    prototype   = "data.frame"
  ),
  prototype = list(
    children    = structure(list(), names = character(0L)),
    fold_status = "unfolded",
    prototype   = data.frame()
  )
)

setClass("no_prototype_node",
  slots = c(
    children    = "list",
    fold_status = "character"
  ),
  prototype = list(
    children    = structure(list(), names = character(0L)),
    fold_status = "unfolded"
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
  check_consistent_children <- function(object) {
    if (length(object@children) >= 2L) {
      child_prototypes <- map(object@children, extract_prototype)
      for (i in 2L:length(child_prototypes)) {
        result <- all.equal(
          target           = child_prototypes[1L],
          current          = child_prototypes[i],
          check.attributes = FALSE
        )
        if (! isTRUE(result)) {
          return(FALSE)
        }
      }
    }
    TRUE
  }
  check_consistent_parentwithchildren <- function(object) {
    if (length(object@children) >= 1L) {
      child_prototype <- extract_prototype(object@children[[1L]])
      result <- all.equal(
        target           = object@prototype,
        current          = child_prototype,
        check.attributes = FALSE
      )
      if (! isTRUE(result)) {
        return(FALSE)
      }
    }
    TRUE
  }
  children_names <- names(object@children)
  if (is.null(children_names)) {
    return("@children is required to be a named list")
  }
  else if (any(is.na(children_names))) {
    return("@children names may not contain any missing values")
  }
  else if (length(unique(children_names)) != length(children_names)) {
    return("@children names must be unique")
  }
  else if (! all(map_lgl(object@children, is, "worklogs"))) {
    return("@children must all be worklogs")
  }
  else if (! is_string(object@fold_status)) {
    return("@fold_status must be a string")
  }
  else if (! (object@fold_status %in% c("folded", "unfolded"))) {
    return("@fold_status of %s is invalid", object@fold_status)
  }
  else if (nrow(object@prototype) != 0L) {
    return("@prototype must have 0 rows")
  }
  else if (! check_consistent_children(object)) {
    return("child prototypes must be consistent")
  }
  else if (! check_consistent_parentwithchildren(object)) {
    return("parent prototype must be consistent with children")
  }
  TRUE
}

setValidity("worklogs_node", validity_worklogs_node)

validity_worklogs_leaf <- function(object) {
  check_config <- function(object) {
    TRUE
  }
  if (! is_string(object@name)) {
    return("@name is required to be a string")
  }
  description_label <- object@config@labels@description
  if (! (description_label %in% names(object@worklogs))) {
    return("@config@labels@description is not in the worklogs data frame")
  }
  if (! (is_chr_nomiss(object@worklogs[[description_label]]))) {
    return("the description field must be a character vector without any NAs")
  }
  if (length(unique(object@worklogs[[description_label]])) >= 2L) {
    return("the description field cannot contain more than one value")
  }
  start_label <- object@config@labels@start
  if (! is.null(start_label)) {
    if (! (start_label %in% names(object@worklogs))) {
      return("@config@labels@start is not in the worklogs data frame")
    }
    if (! (inherits(object@worklogs[[start_label]], "POSIXct"))) {
      return("the start field must have class POSIXct")
    }
  }
  end_label <- object@config@labels@end
  if (! is.null(end_label)) {
    if (! (end_label %in% names(object@worklogs))) {
      return("@config@labels@end is not in the worklogs data frame")
    }
    if (! (inherits(object@worklogs[[end_label]], "POSIXct"))) {
      return("the start field must have class POSIXct")
    }
  }
  duration_label <- object@config@labels@duration
  if (! is.null(duration_label)) {
    if (! (duration_label %in% names(object@worklogs))) {
      return("@config@labels@duration is not in the worklogs data frame")
    }
    if (! (inherits(object@worklogs[[duration_label]], "difftime"))) {
      return("the duration field must have class difftime")
    }
  }
  TRUE
}

setValidity("worklogs_leaf", validity_worklogs_leaf)

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
