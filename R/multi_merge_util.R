#' Merge a List of Data Frames by a Common Column
#'
#' Merges a named list of data frames by a shared key (e.g., "ID").
#'
#' @param df_list A named list of data frames.
#' @param by Character name of the column to merge by (must exist in all data frames). Default is "ID".
#' @param include_source Logical. If TRUE, adds a `source` column to identify the origin of rows (only used if merging fails and fallbacks to rbind). Default is FALSE.
#'
#' @return A merged data frame with all columns merged by the key column.
#'
#' @examples
#' merge_dfs_by_id(list_synch_dfs, by = "ID")
#'
#' @export
multi_merge <- function(df_list, by = "ID", include_source = FALSE) {
  if (!is.list(df_list) || !all(sapply(df_list, is.data.frame))) {
    stop("Input must be a list of data frames.")
  }
  if (!all(sapply(df_list, function(df) by %in% colnames(df)))) {
    stop(paste("All data frames must contain the column", by))
  }

  merged_df <- Reduce(function(x, y) merge(x, y, by = by, all = TRUE), df_list)
  return(merged_df)
}
