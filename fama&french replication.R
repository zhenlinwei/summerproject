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

# apply_to_each_month = function(data) {
#   processed_data_list = list()
#   all_months = unique(month(data$Date))
#   
#   for (m in all_months) {
#     month_data = data[month(Date) == m]
#     month_data[, ":="(wt = meTotal/sum(meTotal, na.rm = TRUE)), by = Date]
#     month_data = month_data[,.(Date, id, ret, meTotal, book, wt)]
#     merged_data_month = data[month(Date) == m]
#     merged_data_month = merge(merged_data_month, month_data[, .(Date, id, wt)], on = c("Date", "id"), all.x = TRUE)
#     merged_data_month[,.(Date, id, ret, meTotal, book, wt)]
#     merged_data_month[, ":="(lag_wt = lag(wt, 1)), by = id]
#     merged_data_month[, ":="(drifted_wt = lag_wt * (1 + ret))]
#     merged_data_month[, ":="(normal_drifted_wt = drifted_wt / sum(drifted_wt, na.rm = TRUE)), by = Date]
#     merged_data_month[, ":="(wt = ifelse(!is.na(wt), wt, normal_drifted_wt))]
#     processed_data_list[[as.character(m)]] = merged_data_month
#   }
#   processed_data = do.call(rbind, processed_data_list)
#   return(processed_data)
# }
# 
# #Return price drifted data
# new_merged_data = apply_to_each_month(merged_data)

rebalance = function(data, rebMonth) {
  # Args:
  #   data:     data.table; the data that contains Date, id, ret, meTotal
  #   rebMonth: numeric; 1~12, the number indicates rebalance to happen at 
  #             the beginning of the month. This can be an array 
  invisible(sapply(rebMonth, function(x) stopifnot(x>0, x<=12))) 
  temp = copy(data)
  temp[, ":="(lagMe = lag(meTotal, 1)), by = id]
  
  for (i in 1:length(rebMonth)) {
    # find corresponding drifting months
    if (i == length(rebMonth)) nextReb = rebMonth[1] else nextReb = rebMonth[i+1]
    if(nextReb > rebMonth[i]) driftMonths =  (rebMonth[i]):(nextReb-2)+1 else driftMonths = ((rebMonth[i]):(nextReb+10)) %% 12 + 1
    
    temp[month(Date) == rebMonth[i], ":="(wt = lagMe/sum(lagMe, na.rm = TRUE)), by = Date]
    temp[, ":="(lagWt = lag(wt, 1)), by = id]
    for (j in 1:length(driftMonths)){
      temp[month(Date) == driftMonths[j], ":="(wt = (lagWt * (1 + ret))/sum(lagWt * (1 + ret), na.rm = T)), by = Date]
      temp[, ":="(lagWt = lag(wt, 1)), by = id]
    }
  }
  temp$lagWt = NULL
  return(temp)
}

#Return price drifted data
new_merged_data = rebalance(merged_data, 7)

#Calculate the portfolio return, filtering out missing values.
new_merged_data_filtered = new_merged_data[complete.cases(new_merged_data$wt),]
portfolio_return = new_merged_data_filtered[,.(MKT = weighted.mean(ret, wt)), by = Date]
setorder(portfolio_return, "Date")

ff3 = as.data.table(read.csv("FF3 replicated.csv"))
mktCheck = merge(ff3[,.(Date = ymd(Date), MKT)], portfolio_return, by = "Date", all.x = T)
cor(mktCheck$MKT.x, mktCheck$MKT.y)

#Classifying size and value into 6 categories.
#Column "size": 0 = small, 1 = big
#Column :value": 0 = growth, 1 = neutral, 2 = value
merged_data[,.(Date, id, ret, meTotal, book)]
june_data = merged_data[month(Date) == 6]
june_data[, ":="(wt = meTotal/sum(meTotal, na.rm = TRUE)), by = Date]
june_data[,.(Date, id, ret, meTotal, book, wt)]
                   
percentile_50 = quantile(june_data$meTotal, probs = 0.5, na.rm = TRUE)
june_data = june_data %>%
  mutate(size = ifelse(meTotal <= percentile_50, "0", "1"))

june_data = june_data %>%
  mutate(value = book / meTotal)

percentile_30 = quantile(june_data$value, probs = 0.3, na.rm = TRUE)
percentile_70 = quantile(june_data$value, probs = 0.7, na.rm = TRUE)

june_data = june_data %>%
  mutate(value = case_when(
    value <= percentile_30 ~ "0",
    value > percentile_30 & value <= percentile_70 ~ "1",
    TRUE ~ "2"
  ))

                   
