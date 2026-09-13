
library(dplyr)

df<- read.csv("src/modules/machine_learning/ml_data/daily_hourly_historic_energy_source.csv",stringsAsFactors = FALSE)




RENEWABLE_FUELS <- c("GEO", "SNB", "SUN", "WAT", "WND")

renewables_sum <- df %>%
  filter(fueltype %in% RENEWABLE_FUELS) %>%
  group_by(period) %>%
  summarise(sum = sum(value, na.rm = TRUE), .groups = "drop")

head(renewables_sum)


write_csv(renewables_sum,"buenos_dias_malos_dias.csv")