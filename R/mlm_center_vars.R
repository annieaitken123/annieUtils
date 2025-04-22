#' Center Variables for Multilevel Modeling (CM and CWC)
#'
#' This function calculates both between-person (CM) and within-person (CWC)
#' centered versions of the specified variables within a dataframe.
#'
#' @param data A data frame containing the variables to center.
#' @param id_var A string specifying the column name of the grouping variable (e.g., participant ID).
#' @param vars A character vector of variable names to be centered.
#'
#' @return The original data frame with new columns added for each variable:
#'   - `var_cm`: the between-person mean for each subject
#'   - `var_cwc`: the within-person centered value (original - between-person mean)
#'
#' @examples
#' df_centered <- center_variables(data = mydata, id_var = "ID", vars = c("rsa.power", "theta"))
center_variables <- function(data, id_var, vars) {
  require(dplyr)
  
  # Step 1: Calculate CM (between-person means)
  cm_df <- data %>%
    group_by(.data[[id_var]]) %>%
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE), .names = "{.col}_cm"), .groups = "drop")
  
  # Step 2: Join CM back to original data
  data <- data %>% left_join(cm_df, by = id_var)
  
  # Step 3: Calculate CWC (within-person centered)
  for (var in vars) {
    cm_col <- paste0(var, "_cm")
    cwc_col <- paste0(var, "_cwc")
    data[[cwc_col]] <- data[[var]] - data[[cm_col]]
  }
  
  return(data)
}
