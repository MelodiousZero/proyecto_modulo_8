
library(EIAapi)
library(dotenv)
library(httr)
library(jsonlite)

library(httr)
library(jsonlite)
library(sf)
library(dplyr)

if (file.exists(".env")) {
  load_dot_env(file = ".env")
}

api_key <- Sys.getenv("eia_key")

offsets <- seq(0, 200998, by = 5000)



datos <- NULL

for (off in offsets){
  
  resp <- GET(
    "https://api.eia.gov/v2/electricity/rto/fuel-type-data/data/",
    query = list(
      api_key=api_key,
      frequency="hourly",
      `data[0]`            = "value",
      `facets[fueltype][]`     = "GEO",
      `facets[fueltype][]`     = "SNB",
      `facets[fueltype][]`     = "SUN",
      `facets[fueltype][]`     = "WAT",
      `facets[fueltype][]`     = "WND",
      `facets[respondent][]` = "CISO",
      `sort[0][column]`    = "period",
      `sort[0][direction]` = "desc",
      offset               = off,
      length               = 5000
    )
  )
  datos <- bind_rows(datos, fromJSON(content(resp, "text"))$response$data)
  Sys.sleep(0.3)
}

write.csv(datos, "machine_learning/ml_data/daily_hourly_historic_energy_source.csv", row.names = FALSE)


