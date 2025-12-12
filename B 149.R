library(readr)    
library(dplyr)     
library(ggplot2)   

df <- read_csv("flight_data_DEL_BLR.csv")

df <- df %>%
  mutate(Price_int = as.numeric(gsub(",", "", Price)))

df <- df %>%
  mutate(
    Duration_min = {
  
      m_h  <- as.numeric(sub("^(\\d+).*", "\\1", gsub(" h.*", "", Duration)))
      m_m  <- as.numeric(sub("^(\\d+).*", "\\1", gsub(".*h \\s*", "", gsub(" m", "", Duration))))
      m_h * 60 + m_m
    }
  )
airline_summary <- df %>%
  group_by(FlightName) %>%
  summarise(
    mean_price = mean(Price_int, na.rm = TRUE),
    sd_price   = sd(Price_int, na.rm = TRUE),
    n          = n()
  ) %>%
  ungroup()

full_service <- c("Air India", "Vistara")
df <- df %>%
  mutate(CarrierType = if_else(FlightName %in% full_service, "Full-service", "Low-cost"))
