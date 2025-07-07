#' Deduplicate a data frame by ID, keeping the first occurrence
#'
#' @param df A data frame containing an ID column.
#' @param id_col Character. Name of the ID column (default: "ID").
#'
#' @return A data frame with only the first instance of each ID.
#' @export
dedup_keep_first <- function(df, id_col = "ID") {
  if (!id_col %in% names(df)) {
    stop(paste("Column", id_col, "not found in data frame."))
  }

  df <- df[!is.na(df[[id_col]]), , drop = FALSE]

  dedup_df <- df[!duplicated(df[[id_col]]), ]
  return(dedup_df)
}
