library(readxl)
library(dplyr)
library(zoo)
library(data.table)
library(lubridate)
financial_data = read_excel("financialdata excel.xlsx")
market_data = read_excel("marketdata excel.xlsx")
financial_data = as.data.table(read_excel("financialdata excel.xlsx"))
market_data = as.data.table(read_excel("marketdata excel.xlsx"))

#merge financial and market data
financial_data[, annDate := as.Date(annDate)]
market_data[, Date := as.Date(Date)]
financial_data = financial_data[, rankFdate := rank(-as.numeric(fdate)), by = .(id, annDate)][rankFdate == 1]
financial_data[, Date := annDate]
merged_data = financial_data[market_data, on = .(id, Date), roll = Inf]

#Tryout for price drifting of June
merged_data[,.(Date, id, ret, meTotal, book)]
june_data = merged_data[month(Date) == 6]
june_data[, ":="(wt = meTotal/sum(meTotal, na.rm = TRUE)), by = Date]
june_data[,.(Date, id, ret, meTotal, book, wt)]
new_merged_data = merged_data[june_data[, .(Date, id, wt)], on = .(Date, id)][, .(Date, id, ret, meTotal, book, wt)]
new_merged_data[, ":="(lag_wt = lag(wt, 1)), by = id]
new_merged_data[, ":="(drifted_wt = lag_wt*(1+ret))]
new_merged_data[, ":="(normal_drifted_wt = drifted_wt/sum(drifted_wt, na.rm = TRUE)), by = Date]
new_merged_data[, ":="(wt = ifelse(!is.na(wt), wt, normal_drifted_wt))]

#Price drifting of each month
merged_data$Date = as.Date(merged_data$Date)
merged_data[,.(Date, id, ret, meTotal, book)]
apply_to_each_month = function(data) {
  processed_data_list = list()
  all_months = unique(month(data$Date))
  
  for (m in all_months) {
    month_data = data[month(Date) == m]
    month_data[, ":="(wt = meTotal/sum(meTotal, na.rm = TRUE)), by = Date]
    month_data = month_data[,.(Date, id, ret, meTotal, book, wt)]
    merged_data_month = data[month(Date) == m]
    merged_data_month = merge(merged_data_month, month_data[, .(Date, id, wt)], on = c("Date", "id"), all.x = TRUE)
    merged_data_month[,.(Date, id, ret, meTotal, book, wt)]
    merged_data_month[, ":="(lag_wt = lag(wt, 1)), by = id]
    merged_data_month[, ":="(drifted_wt = lag_wt * (1 + ret))]
    merged_data_month[, ":="(normal_drifted_wt = drifted_wt / sum(drifted_wt, na.rm = TRUE)), by = Date]
    merged_data_month[, ":="(wt = ifelse(!is.na(wt), wt, normal_drifted_wt))]
    processed_data_list[[as.character(m)]] = merged_data_month
  }
  processed_data = do.call(rbind, processed_data_list)
  return(processed_data)
}

#Return price drifted data
new_merged_data = apply_to_each_month(merged_data)

#Calculate the portfolio return, filtering out missing values.
new_merged_data_filtered = new_merged_data[complete.cases(new_merged_data$ret, new_merged_data$normal_drifted_wt), ]
portfolio_return = weighted.mean(new_merged_data_filtered$ret, new_merged_data_filtered$normal_drifted_wt)
