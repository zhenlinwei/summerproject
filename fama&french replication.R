library(readxl)
financial_data = read_excel("financialdata excel.xlsx")
market_data = read_excel("marketdata excel.xlsx")
library(dplyr)
library(zoo)
library(data.table)
library(lubridate)

financial_data$annDate = as.Date(financial_data$annDate)
market_data$Date = as.Date(market_data$Date)
financial_data = financial_data[order(financial_data$annDate, decreasing = TRUE), ]

merged_data = data.frame()
for (i in 1:nrow(market_data)) {
  id = market_data$id[i]
  date = market_data$Date[i]
}
closest_date = financial_data$annDate[financial_data$id == id][which.max(financial_data$annDate[financial_data$id == id] <= date)]
financial_row = financial_data[financial_data$id == id & financial_data$annDate == closest_date, ]
merged_row = merge(financial_row, market_data[i, ], by = "id")

merged_data = rbind(merged_data, merged_row)

rownames(merged_data) = NULL
print(merged_data)
