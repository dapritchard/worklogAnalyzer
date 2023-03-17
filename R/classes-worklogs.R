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