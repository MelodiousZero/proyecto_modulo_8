# 5. Combine all pages into one data frame ---------------------------------
if (length(all_records) > 0) {
  final_df <- do.call(rbind, all_records)
} else {
  warning("No records were retrieved.")
  final_df <- data.frame()
}

# --- FIX: handle list columns before writing ---
# Identify list columns
list_cols <- names(final_df)[sapply(final_df, is.list)]

if (length(list_cols) > 0) {
  message("List columns detected and converted to JSON strings: ",
          paste(list_cols, collapse = ", "))
  for (col in list_cols) {
    final_df[[col]] <- vapply(
      final_df[[col]],
      function(x) {
        if (is.null(x) || length(x) == 0) {
          NA_character_
        } else {
          paste0(toJSON(x, auto_unbox = TRUE, null = "null"), collapse = "")
        }
      },
      character(1)
    )
  }
}

# Safety: make sure no list columns remain
stopifnot(!any(sapply(final_df, is.list)))

# 6. Write to CSV -----------------------------------------------------------
output_file <- "src/data/odin_real_time_outages_county.csv"
write.csv(final_df, file = output_file, row.names = FALSE, na = "")

message("Data saved to: ", output_file)
message("Rows: ", nrow(final_df), " | Columns: ", ncol(final_df))