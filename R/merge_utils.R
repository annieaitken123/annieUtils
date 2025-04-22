#' Merge Multiple Data Files or Data Frames
#'
#' Merges multiple .txt/.csv/.tsv files or a list of data frames into a single data frame.
#' Optionally includes source identifiers and can write the output to a file.
#'
#' @param input Either a character vector of file paths OR a named list of data frames.
#' @param include_source Logical, whether to include a 'source' column. If input is file paths,
#'        it will be the file name; if it's a list, the list names. Default is TRUE.
#' @param write_output Logical, whether to write the combined data to a file. Default is FALSE.
#' @param output_file Character, path for the output file if write_output is TRUE. Default is "merged_output.txt".
#' @param sep Separator for input/output files. Use "," for CSV, "\t" for tab-delimited, etc. Default is tab.
#' @param file_type If input is file paths, specify "txt", "csv", "tsv", or "custom". Default is "txt".
#'
#' @return A single data frame resulting from the merge.
#'
#' @examples
#' merge_data_files(input = LB_393_files, file_type = "txt")
#' merge_data_files(input = list(df1 = df1, df2 = df2), include_source = TRUE)
merge_data_files <- function(input,
                             include_source = TRUE,
                             write_output = FALSE,
                             output_file = "merged_output.txt",
                             sep = "\t",
                             file_type = "txt") {

  # Determine input type: file paths (character vector) or list of data frames
  is_file_list <- is.character(input)
  is_df_list <- is.list(input) && all(sapply(input, is.data.frame))

  if (!is_file_list && !is_df_list) {
    stop("Input must be either a character vector of file paths or a named list of data frames.")
  }

  # Read and merge files
  if (is_file_list) {
    # Define file reader based on file_type
    read_fun <- switch(tolower(file_type),
                       "csv" = function(f) read.csv(f, stringsAsFactors = FALSE),
                       "tsv" = function(f) read.delim(f, stringsAsFactors = FALSE),
                       "txt" = function(f) read.table(f, header = TRUE, sep = sep, stringsAsFactors = FALSE),
                       "custom" = function(f) read.table(f, header = TRUE, sep = sep, stringsAsFactors = FALSE),
                       stop("Unsupported file_type. Choose from 'txt', 'csv', 'tsv', or 'custom'."))

    # Apply the reader and optionally add source
    merged_df <- do.call(rbind, lapply(input, function(file) {
      df <- read_fun(file)
      if (include_source) {
        df$source <- basename(file)
      }
      return(df)
    }))

  } else if (is_df_list) {
    # Combine from a named list of data frames
    merged_df <- do.call(rbind, Map(function(df, name) {
      if (include_source) {
        df$source <- name
      }
      return(df)
    }, input, names(input)))
  }

  # Optionally write to file
  if (write_output) {
    write.table(merged_df, file = output_file, sep = sep, row.names = FALSE, quote = FALSE)
    message(paste("Merged data written to", output_file))
  }

  return(merged_df)
}
