#1
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


#2
library(readxl)
financial_data = read_excel("financialdata excel.xlsx")
market_data = read_excel("marketdata excel.xlsx")
library(dplyr)
library(zoo)
library(data.table)
library(lubridate)

financial_data$annDate = as.Date(financial_data$annDate)
market_data$Date = as.Date(market_data$Date)

merged_data = data.frame()
for (i in 1:nrow(market_data)) {
  id = market_data$id[i]
  specific_date = market_data$Date[i]
  
  close_dates = financial_data$annDate[financial_data$id == id & financial_data$annDate <= specific_date]
  
  if (length(close_dates) > 0) {
    closest_date = max(close_dates)
    closest_book = financial_data$book[financial_data$id == id & financial_data$annDate == closest_date]
    
    merged_data <- rbind(merged_data, data.frame(id = id, date = specific_date, closest_date = closest_date, book = closest_book))
  }
}

print(merged_data)

#3
library(data.table)
library(lubridate)
library(readxl)

financial_data = as.data.table(read_excel("financialdata excel.xlsx"))
market_data = as.data.table(read_excel("marketdata excel.xlsx"))

financial_data[, Date := as.Date(annDate)]
market_data[, Date := as.Date(Date)]

merged_data = financial_data[market_data, on = .(id, Date), roll = Inf]
# date check
merged_data[as.Date(annDate) > Date] # if it returns any row, then something is wrong
