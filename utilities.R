create_node_from_leaf <- function(wkls) {
  stopifnot(is(wkls, "worklogs_leaf"))
  new(
    Class       = "worklogs_node",
    children    = set_names(list(wkls), wkls@name),
    fold_status = "unfolded",
    prototype   = wkls@worklogs[numeric(0L), ]
  )
}
