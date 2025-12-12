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
full_prices <- df %>% filter(CarrierType == "Full-service") %>% pull(Price_int)
low_prices  <- df %>% filter(CarrierType == "Low-cost")  %>% pull(Price_int)
t_test_result <- t.test(full_prices, low_prices, var.equal = FALSE)
print(t_test_result)

p1 <- ggplot(airline_summary, aes(x = FlightName, y = mean_price)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_price - sd_price,
                    ymax = mean_price + sd_price),
                width = 0.2) +
  labs(title = "Average Flight Price by Airline",
       x = "Airline",
       y = "Average Price (INR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)

p2 <- ggplot(df, aes(x = Price_int)) +
  geom_histogram(binwidth = 500, boundary = 0, closed = "left") +
  labs(title = "Distribution of Flight Prices (Delhi to Bangalore)",
       x = "Price (INR)",
       y = "Number of Flights") +
  theme_minimal()


print(p2)

