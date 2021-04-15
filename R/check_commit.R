#' Check Recursively Directories For Uncommitted
#'
#' @param root_directory A directory in which there are git subdirectories.
#' @param group A (vector of) user(s) to keep or exclude with prefix "!".
#' @param group A (vector of) group(s) to keep or exclude with prefix "!"..
#' @param commit A boolean. if `TRUE`, commits uncommitted files.
#' @param push A boolean. if `TRUE`, push commits.
#'
#' @return A tibble
#'
#' @name check_commit
#' @export
check_commit <- function(
  root_directory = ".",
  user = NULL,
  group = NULL,
  commit = FALSE,
  push = FALSE
) {
  dta <- fs::dir_info(path = root_directory, recurse = FALSE)
  dta <- dta[fs::is_dir(dta[["path"]]), c("path", "user", "group", "access_time")]

  if (!is.null(user)) {
    user_keep <- unique(setdiff(dta[["user"]], gsub("^!", "", grep("!", user, fixed = TRUE, value = TRUE))))
    if (any(!grepl("^!", user))) user_keep <- intersect(user_keep, user[!grepl("^!", user)])
    dta <- dta[dta[["user"]] %in% user_keep, ]
  }

  if (!is.null(group)) {
    group_keep <- unique(setdiff(dta[["group"]], gsub("^!", "", grep("!", group, fixed = TRUE, value = TRUE))))
    if (any(!grepl("^!", group))) group_keep <- intersect(group_keep, group[!grepl("^!", group)])
    dta <- dta[dta[["group"]] %in% group_keep, ]
  }

  dta <- dta[sapply(dta[["path"]], function(x) !inherits(try(gert::git_find(path = x), silent = TRUE), "try-error")), ]
  dta <- dta[!grepl("^SB_", basename(dta[["path"]])), ]
  dta[["commit"]] <- sapply(dta[["path"]], function(x) nrow(gert::git_diff(repo = x)) > 0)

  if (commit) {
    sapply(
      X = dta[dta[["commit"]], ][["path"]],
      FUN = function(x) gert::git_commit_all(message = "automatic commit", repo = x)
    )
  }

  if (push) {
    sapply(
      X = dta[["path"]],
      FUN = function(x) try(gert::git_push(repo = x))
    )
  }

  out <- dta[dta[["commit"]], c("path", "user", "group", "access_time")]

  if (nrow(out) > 0) {
    files_tmp <- lapply(out[["path"]], function(x) gert::git_status(repo = x))
    out <- cbind(out[rep(seq_along(files_tmp), sapply(files_tmp, nrow)), ], do.call("rbind", files_tmp))
    class(out) <- c("tbl_df", "tbl", "data.frame")
    out[c("path", "file", "status", "staged", "user", "group", "access_time")]
  } else {
    out[c("path", "user", "group", "access_time")]
  }
}

#' @rdname check_commit
#' @export
cc <- check_commit

