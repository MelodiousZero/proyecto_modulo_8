

library(httr)
library(jsonlite)
library(sf)
library(ggplot2)
library(maps)
library(dplyr)


# ============================================================
# PART A — Fetch all records from the ODIN API
# ============================================================

base_url <- paste0(
  "https://openenergyhub.ornl.gov/api/explore/v2.1/",
  "catalog/datasets/odin-real-time-outages-county/records"
)

fetch_page <- function(offset = 0, limit = 100) {
  url  <- paste0(base_url, "?limit=", limit, "&offset=", offset)
  resp <- GET(url)
  
  if (http_error(resp)) {
    stop("API request failed. Status: ", status_code(resp))
  }
  
  txt  <- content(resp, as = "text", encoding = "UTF-8")
  fromJSON(txt, flatten = TRUE)
}

all_records <- list()
offset      <- 0
limit       <- 100
total       <- NULL

repeat {
  message("Fetching offset = ", offset, " ...")
  page <- fetch_page(offset = offset, limit = limit)
  
  if (is.null(total)) {
    total <- page$total_count
    message("Total records reported by API: ", total)
  }
  
  if (!is.null(page$results) && NROW(page$results) > 0) {
    df_page <- as.data.frame(page$results, stringsAsFactors = FALSE)
    all_records[[length(all_records) + 1]] <- df_page
    retrieved <- sum(vapply(all_records, nrow, integer(1)))
    message("Retrieved ", retrieved, " / ", total)
    if (retrieved >= total || nrow(df_page) < limit) break
  } else {
    break
  }
  
  offset <- offset + limit
  Sys.sleep(0.5)   # be polite to the API
}


# ---- Combine pages ------------------------------------------------------
if (length(all_records) > 0) {
  final_df <- do.call(rbind, all_records)
} else {
  warning("No records were retrieved.")
  final_df <- data.frame()
}


# ---- Fix list columns (nested JSON) -------------------------------------
list_cols <- names(final_df)[vapply(final_df, is.list, logical(1))]

if (length(list_cols) > 0) {
  message("Converting list columns to JSON strings: ",
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

stopifnot(!any(vapply(final_df, is.list, logical(1))))


# ---- Write CSV ----------------------------------------------------------
output_file <- "src/data/odin_real_time_outages_county.csv"

write.csv(final_df, file = output_file, row.names = FALSE, na = "")

message("Saved: ", output_file)
message("Rows: ", nrow(final_df), " | Cols: ", ncol(final_df))

