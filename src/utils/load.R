library(EIAapi)
library(dotenv)
library(httr)
library(jsonlite)
# Pulling the API key from my renviron file


if (file.exists(".env")) {
  load_dot_env(file = ".env")
}

api_key <- Sys.getenv("eia_key")

list_api_paths <- list("electricity/rto/region-data/data/", #Electric Power Operations Daily and Hourly - Hourly demand, Demand Forecast, Generation and interchange
                   "electricity/rto/region-sub-ba-data/data/", #Electric Power Operations Daily and Hourly - Hourly demand by subregion
                   "electricity/rto/fuel-type-data/data/", #Electric Power Operations Daily and Hourly - Hourly Generation by Energy Source
                   "electricity/rto/interchange-data/data/",#Electric Power Operations Daily and Hourly - Hourly Interchange by Neighboring Balancing Authority
                   "electricity/state-electricity-profiles/source-disposition/data/", #State Specific Data - Supply and disposition of electricity
                   "electricity/operating-generator-capacity/data/", #Electricity - Inventory of Operable Genrators 
                   "electricity/state-electricity-profiles/capability/data/" #Electricity - State specific data generating capacity
)




# Function to fetch and save EIA data
fetch_and_save_eia_data <- function(path_index, csv_filename) {
  # Fetch the data
  df <- eia_get(
    api_key = api_key,
    api_path = list_api_paths[[path_index]],
    data = "value"
  )
  
  # Save to CSV
  write.csv(df, csv_filename, row.names = FALSE)
  
  # Return the data frame (optional)
  return(df)
}





df4 <- fetch_and_save_eia_data(
  path_index = 4,
  csv_filename = "src/data/hourly_interchange.csv"
)

# solo los de arriba funcionaron con la paqueteria de eapi

#hourly demand by subregion
response <- GET(
  "https://api.eia.gov/v2/electricity/rto/region-sub-ba-data/data/",
  query = list(
    api_key = api_key,
    frequency = "hourly",
    `data[0]` = "value",
    start = "2019-01-01T00",
    `sort[0][column]` = "period",
    `sort[0][direction]` = "desc",
    offset = 0,
    length = 10000
  )
)

status_code(response)  # Should be 200

parsed <- fromJSON(content(response, "text"))

df <- parsed$response$data


write.csv(df, "src/data/hourly_demand_subregion.csv", row.names = FALSE)


#State Supply Electricity
respuesta <- GET(
  "https://api.eia.gov/v2/electricity/state-electricity-profiles/source-disposition/data/",
  query = list(
    api_key = api_key,
    frequency = "annual",
    `data[0]` = "combined-heat-and-pwr-comm",
    `data[1]` = "combined-heat-and-pwr-elect",
    `data[2]` = "combined-heat-and-pwr-indust",
    `data[3]` = "direct-use",
    `data[4]` = "elect-pwr-sector-gen-subtotal",
    `data[5]` = "electric-utilities",
    `data[6]` = "energy-only-providers",
    `data[7]` = "estimated-losses",
    `data[8]` = "facility-direct",
    `data[9]` = "full-service-providers",
    `data[10]` = "independent-power-producers",
    `data[11]` = "indust-and-comm-gen-subtotal",
    `data[12]` = "net-interstate-trade",
    `data[13]` = "net-trade-index",
    `data[14]` = "total-disposition",
    `data[15]` = "total-elect-indust",
    `data[16]` = "total-international-exports",
    `data[17]` = "total-international-imports",
    `data[18]` = "total-net-generation",
    `data[19]` = "total-supply",
    `data[20]` = "unaccounted",
    `sort[0][column]` = "period",
    `sort[0][direction]` = "desc",
    offset = 0,
    length = 5000
  )
)

datos <- fromJSON(content(respuesta, "text"))$response$data
write.csv(datos, "src/data/source_disposition_data.csv", row.names = FALSE)


#Operable Generators
respuesta <- GET(
  "https://api.eia.gov/v2/electricity/operating-generator-capacity/data/",
  query = list(
    api_key = api_key,
    frequency = "monthly",
    `data[0]` = "county",
    `data[1]` = "latitude",
    `data[2]` = "longitude",
    `data[3]` = "nameplate-capacity-mw",
    `data[4]` = "net-summer-capacity-mw",
    `data[5]` = "net-winter-capacity-mw",
    `data[6]` = "operating-year-month",
    `data[7]` = "planned-derate-summer-cap-mw",
    `data[8]` = "planned-derate-year-month",
    `data[9]` = "planned-retirement-year-month",
    `data[10]` = "planned-uprate-summer-cap-mw",
    `data[11]` = "planned-uprate-year-month",
    `sort[0][column]` = "period",
    `sort[0][direction]` = "desc",
    offset = 0,
    length = 5000
  )
)

datos <- fromJSON(content(respuesta, "text"))$response$data
write.csv(datos, "src/data/operating_generators_capacity.csv", row.names = FALSE)


#State gnerating capacity
respuesta <- GET(
  "https://api.eia.gov/v2/electricity/state-electricity-profiles/capability/data/",
  query = list(
    api_key = api_key,
    frequency = "annual",
    `data[0]` = "capability",
    `sort[0][column]` = "period",
    `sort[0][direction]` = "desc",
    offset = 0,
    length = 5000
  )
)

datos <- fromJSON(content(respuesta, "text"))$response$data
write.csv(datos, "src/data/state_generator_capacity.csv", row.names = FALSE)

#Electricity Sales to Ultimate Customoers - Quarterly

respuesta <- GET(
  "https://api.eia.gov/v2/electricity/retail-sales/data/",
  query = list(
    api_key = api_key,
    frequency = "quarterly",
    `data[0]` = "customers",
    `data[1]` = "price",
    `data[2]` = "revenue",
    `data[3]` = "sales",
    `sort[0][column]` = "period",
    `sort[0][direction]` = "desc",
     offset = 0,
     length = 5000

  )
)

datos <- fromJSON(content(respuesta, "text"))$response$data
write.csv(datos, "src/data/sales_to_customers_quarterly.csv", row.names = FALSE)

#Electricity Sales to Ultimate Customoers - Monthly

respuesta <- GET(
  "https://api.eia.gov/v2/electricity/retail-sales/data/",
  query = list(
    api_key = api_key,
    frequency = "monthly",
    `data[0]` = "customers",
    `data[1]` = "price",
    `data[2]` = "revenue",
    `data[3]` = "sales",
    `sort[0][column]` = "period",
    `sort[0][direction]` = "desc",
    offset = 0,
    length = 5000
    
  )
)

datos <- fromJSON(content(respuesta, "text"))$response$data
write.csv(datos, "src/data/sales_to_customers_monthly.csv", row.names = FALSE)


offsets <- c(0,5000,10000,15000)
datos <- NULL

for (off in offsets) {
  
  resp <- GET(
    "https://api.eia.gov/v2/electricity/rto/region-data/data/",
    query = list(
      api_key              = api_key,
      frequency            = "hourly",
      `data[0]`            = "value",
      `sort[0][column]`    = "period",
      `sort[0][direction]` = "desc",
      offset               = off,
      length               = 5000
    )
  )
  datos <- bind_rows(datos, fromJSON(content(resp, "text"))$response$data)
  Sys.sleep(0.3)
}

write.csv(datos, "src/data/daily_hourly_all.csv", row.names = FALSE)


datos <- NULL

for (off in offsets){
    
  resp <- GET(
    "https://api.eia.gov/v2/electricity/rto/fuel-type-data/data/",
    query = list(
      api_key=api_key,
      frequency="hourly",
      `data[0]`            = "value",
      `sort[0][column]`    = "period",
      `sort[0][direction]` = "desc",
      offset               = off,
      length               = 5000
    )
  )
  datos <- bind_rows(datos, fromJSON(content(resp, "text"))$response$data)
  Sys.sleep(0.3)
}

write.csv(datos, "src/data/hourly_generation_by_energy_source.csv", row.names = FALSE)





# ODIN outages! 



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



