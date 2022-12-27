add_hierarchy <- function(x, hierarchy) {
  stopifnot(is.list(x) || is.data.frame(x))
  `if`(
    is.data.frame(x),
    add_hierarchy_leaf(x, hierarchy),
    add_hierarchy_node(x, hierarchy)
  )
}

add_hierarchy_node <- function(worklogs_leaf, hierarchy) {
  stopifnot(
    is_worklogs_node(worklogs_node),
    is.list(hierarchy) || is.character(hierarchy)
  )
  `if`(
    is.list(hierarchy),
    add_hierarchy_node_list(worklogs_node, hierarchy),
    add_hierarchy_node_char(worklogs_node, hierarchy)
  )
}

add_hierarchy_node_char <- function(worklogs_node, hierarchy) {
  stopifnot(
    is_worklogs_node(worklogs_node),
    is.character(hierarchy),
    ! is.na(hierarchy),
    length(hierarchy) == length(worklogs_node)
  )
  worklogs(split(worklogs_node, hierarchy))
}

add_hierarchy_node_list <- function(worklogs_node, hierarchy) {
  stop("add_hierarchy_node_list is not yet implemented")
}

add_hierarchy_leaf <- function(worklogs_leaf, hierarchy) {
  stopifnot(
    is_worklogs_leaf(worklogs_leaf),
    is.list(hierarchy) || is.character(hierarchy)
  )
  `if`(
    is.list(hierarchy),
    add_hierarchy_leaf_list(worklogs_leaf, hierarchy),
    add_hierarchy_leaf_char(worklogs_leaf, hierarchy)
  )
}

add_hierarchy_leaf_char <- function(worklogs_leaf, hierarchy) {
  stopifnot(
    is_worklogs_leaf(worklogs_leaf),
    is.character(hierarchy),
    ! is.na(hierarchy),
    length(hierarchy) == nrow(worklogs_leaf)
  )
  worklogs(split(worklogs_leaf, hierarchy))
}

add_hierarchy_leaf_list <- function(worklogs_leaf, hierarchy) {
  stop("add_hierarchy_leaf_list is not yet implemented")
}

hierarchy_from_elements <- function(worklogs_df) {
  split_children <- function(worklogs_df) {
    parent <- map_chr(worklogs_df$parents, `[`, 1L)
    worklogs_df$parents <- map(worklogs_df$parents, `[`, -1L)
    children <- split(worklogs_df, parent)
    structure(
      .Data = map(children, hierarchy_from_elements),
      class = "worklogs_node"
    )
  }
  stopifnot(
    is.data.frame(worklogs_df),
    "parents" %in% names(worklogs_df),
    is.list(worklogs_df$parents),
    map_lgl(worklogs_df$parents, is.character)
  )
  has_parents <- map_int(worklogs_df$parents, length) >= 1L
  wkls <- c(
    split_children(worklogs_df[has_parents, ]),
    mk_worklogs_leafs(worklogs_df[! has_parents, ])
  )
  structure(wkls, class = "worklogs_node")
}

worklogs_from_parents <- function(worklogs_df) {
  split_children <- function(worklogs_df) {
    parent <- map_chr(worklogs_df$parents, `[`, 1L)
    worklogs_df$parents <- map(worklogs_df$parents, `[`, -1L)
    raw_children <- split(worklogs_df, parent)
    map(raw_children, worklogs_from_parents)
  }
  stopifnot(
    is.data.frame(worklogs_df), "parents" %in% names(worklogs_df),
    is.list(worklogs_df$parents),
    map_lgl(worklogs_df$parents, is.character)
  )
  has_parents <- map_int(worklogs_df$parents, length) >= 1L
  worklogs_leafs <- mk_worklogs_leafs(worklogs_df[! has_parents, ], TRUE)
  children <- c(
    split_children(worklogs_df[has_parents, ]),
    worklogs_leafs@children
  )
  new("worklogs_node", children = children)
}
