#' Configuration Classes
#'
#' S4 classes used in specifying worklogs configuration information.
#'
#' @slot description A string specifying the column name of the worklog
#'   description.
#' @slot start A string specifying the column name of the worklog start times.
#' @slot end A string specifying the column name of the worklog end times.
#' @slot duration A string specifying the column name of the worklog durations.
#' @slot tags A string specifying the column name of the worklog tags.
#'
#' @name classes_worklogs_config
NULL

#' @rdname classes_worklogs_config
#' @export
setClass("config_labels",
  slots = c(
    description = "character",
    start       = "character",
    end         = "character",
    duration    = "character",
    tags        = "character"
 ),
 prototype = list(
    description = "",
    start       = "",
    end         = "",
    duration    = "",
    tags        = ""
  )
)

# #' @rdname configuration_classes
# #' @export
# setClass("config_formats",
#   slots = c(
#     start    = "character",
#     end      = "character",
#     duration = "character"
#  ),
#  prototype = list(
#     start    = NA_character_,
#     end      = NA_character_,
#     duration = NA_character_
#   )
# )

#' @slot labels A `config_labels` object.
#' @rdname classes_worklogs_config
#' @export
setClass("worklogs_config",
  slots     = c(labels = "config_labels"),
  prototype = list(labels = new("config_labels"))
)

#' Create a `worklogs_config` Object
#'
#' This is the intended mechanism for creating `worklogs_config` objects.
#'
#' @param description_label A string specifying the column name of the worklog
#'   descriptions.
#' @param start_label Either a string specifying the column name of the worklog
#'   start times, or `NA_character_` if there is no such column.
#' @param end_label Either a string specifying the column name of the worklog
#'   end times, or `NA_character_` if there is no such column.
#' @param duration_label Either a string specifying the column name of the
#'   worklog duration entries, or `NA_character_` if there is no such column.
#' @param tags_label Either a string specifying the column name of the worklog
#'   tags, or `NA_character_` if there is no such column.
#'
#' @return A `worklogs_config` object.
#'
#' @export
worklogs_config <- function(description_label,
                            start_label = NA_character_,
                            end_label = NA_character_,
                            duration_label = NA_character_,
                            tags_label = NA_character_) {
  # TODO: check the following:
  # - at least 2 out of 3 of start_label, end_label, and end_format
  stopifnot(
    is_string(description_label),
    is_maybe_string(start_label),
    is_maybe_string(end_label),
    is_maybe_string(duration_label),
    is_maybe_string(tags_label)
  )
  new("worklogs_config",
    labels = new("config_labels",
      description = description_label,
      start       = start_label,
      end         = end_label,
      duration    = duration_label,
      tags        = tags_label
    )# ,
    # formats = new("config_formats",
    #   start    = start_format,
    #   end      = end_format,
    #   duration = duration_format
    # ),
    # timezone = timezone
  )
}

# # TODO: rewrite these as tests of `new` for `config`
# # Ensure that at least two out of three of the start, end, and duration
# # columns are specified
# expect_error(
#   new(
#     Class = "worklogs_leaf",
#     worklogs = install_r_withtags,
#     name = "Install latest version of R",
#     config = worklogs_config_withdefaults(
#       start_label = NA_character_,
#       end_label   = NA_character_
#     )
#   )
# )
# expect_error(
#   new(
#     Class = "worklogs_leaf",
#     worklogs = install_r_withtags,
#     name = "Install latest version of R",
#     config = worklogs_config_withdefaults(
#       start_label    = NA_character_,
#       duration_label = NA_character_
#     )
#   )
# )
# expect_error(
#   new(
#     Class = "worklogs_leaf",
#     worklogs = install_r_withtags,
#     name = "Install latest version of R",
#     config = worklogs_config_withdefaults(
#       end_label      = NA_character_,
#       duration_label = NA_character_
#     )
#   )
# )
