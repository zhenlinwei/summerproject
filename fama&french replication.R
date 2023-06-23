financial_data = read_excel("financialdata excel.xlsx")
market_data = read_excel("marketdata excel.xlsx")
library(dplyr)
library(zoo)
library(lubridate)
financial_data$annDate = as.Date(financial_data$annDate)
market_data$Date = as.Date(market_data$Date)
financial_data$monthly = floor_date(financial_data$annDate, unit = "quarter")
market_data$monthly = make_date(year(market_data$Date), month(market_data$Date))
merged_data = merge(financial_data, market_data, by = c("id","monthly"))
library(tidyr)
merged_data <- merged_data %>%
  group_by(id) %>%
  fill(book) %>%
  ungroup()
