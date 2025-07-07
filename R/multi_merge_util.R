#' Merge a List of Data Frames by a Common Column
#'
#' Merges a named list of data frames by a shared key (e.g., "ID").
#' Ensures the key column is numeric, non-blank, and unique within each data frame before merging.
#'
#' @param df_list A named list of data frames.
#' @param by Character name of the column to merge by (must exist in all data frames). Default is "ID".
#' @param include_source Logical. If TRUE, adds a `source` column (not used here). Default is FALSE.
#'
#' @return A merged data frame with all columns merged by the key column.
#' @export
multi_merge <- function(df_list, by = "ID", include_source = FALSE) {
  if (!is.list(df_list) || !all(sapply(df_list, is.data.frame))) {
    stop("Input must be a list of data frames.")
  }

  if (!all(sapply(df_list, function(df) by %in% colnames(df)))) {
    stop(paste("All data frames must contain the column:", by))
  }

  df_list <- Map(function(df, name) {
    orig_col <- df[[by]]

    # Step 1: Remove rows where ID is blank/empty string/"NA"/spaces
    df <- df[!is.na(orig_col) & trimws(as.character(orig_col)) != "", , drop = FALSE]
    cleaned_col <- df[[by]]  # updated after removing blanks

    # Step 2: Clean and coerce to numeric
    if (is.character(cleaned_col) || is.factor(cleaned_col)) {
      message("Converting character/factor ID to numeric in: ", name)
      numeric_str <- gsub("[^0-9]", "", as.character(cleaned_col))
      coerced <- suppressWarnings(as.numeric(numeric_str))
    } else {
      coerced <- suppressWarnings(as.numeric(cleaned_col))
    }

    # Step 3: Check if coercion introduces new NA
    original_na <- is.na(cleaned_col)
    new_na <- is.na(coerced) & !original_na
    if (any(new_na)) {
      cat("\n❌ Coercion failed for data frame:", name, "\n")
      print(cleaned_col[new_na])
      stop(paste("Column", by, "could not be coerced to numeric in data frame:", name))
    }

    # Step 4: Remove any rows where ID is now NA after coercion
    df <- df[!is.na(coerced), , drop = FALSE]
    df[[by]] <- coerced

    # Step 5: Ensure ID is unique
    if (anyDuplicated(df[[by]])) {
      dup_ids <- df[[by]][duplicated(df[[by]])]
      cat("\n❌ Duplicate IDs found in data frame:", name, "\n")
      print(unique(dup_ids))
      stop(paste("Non-unique IDs detected in", name, "— each data frame must have one row per ID."))
    }

    return(df)
  }, df_list, names(df_list))

  # Merge all cleaned and validated data frames
  merged_df <- Reduce(function(x, y) merge(x, y, by = by, all = TRUE), df_list)
  return(merged_df)
}
