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

setDT(june_data)
setorder(june_data, id, Date)
june_data[, c("book", "ret", "meTotal") := lapply(.SD, na.locf, na.rm = FALSE), by = id]                   
june_data = june_data %>%
  group_by(Date) %>%
  mutate(size_percentile = percent_rank(meTotal))

june_data = june_data %>%
  mutate(beme = book / meTotal)

june_data = june_data %>%
  group_by(Date) %>%
  mutate(value_percentile = percent_rank(beme))

june_data = june_data %>%
  mutate(size = ifelse(size_percentile <= 0.5, "0", "1"))
june_data = june_data %>%
  mutate(value = ifelse(is.na(value_percentile), NA, 
    ifelse(value_percentile <= 0.3, "0",
    ifelse(value_percentile > 0.3 & value_percentile <= 0.7, "1", "2"))))
june_data = june_data %>%
  mutate(size = ifelse(size_percentile <= 0.5, "0", "1"))
june_data = june_data %>%
  mutate(value = ifelse(is.na(value_percentile), NA, 
    ifelse(value_percentile <= 0.3, "0",
    ifelse(value_percentile > 0.3 & value_percentile <= 0.7, "1", "2"))))



# merged_data size and value
#merged_data size and value
merged_data = merged_data %>%
  mutate(Year = if_else(month(Date) < 6, year(Date) - 1, year(Date)))

merged_data = merged_data %>%
  group_by(id, Year) %>%
  mutate(June_meTotal = if_else(month(Date) == 6, meTotal, NA_real_)) %>%
  tidyr::fill(June_meTotal, .direction = "downup") %>%
  ungroup()

merged_data$June_meTotal = na.locf(merged_data$June_meTotal, fromLast = T)

merged_data = merged_data %>%
  group_by(Date) %>%
  mutate(size_percentile = percent_rank(June_meTotal)) %>%
  ungroup()

merged_data = merged_data %>%
  mutate(beme = book / meTotal)
merged_data$beme = na.locf(merged_data$beme, fromLast = T)


merged_data = merged_data %>%
  mutate(Year = if_else(month(Date) < 6, year(Date) - 1, year(Date))) %>%
  group_by(id, Year) %>%
  mutate(June_beme = if_else(month(Date) == 6, beme, NA_real_)) %>%
  tidyr::fill(June_beme, .direction = "downup") %>%
  ungroup()
merged_data$June_beme = na.locf(merged_data$June_beme, fromLast = T)

merged_data = merged_data %>%
  group_by(Date) %>%
  mutate(value_percentile = percent_rank(June_beme)) %>%
  ungroup()



merged_data = merged_data %>%
  mutate(size = ifelse(size_percentile <= 0.5, "0", "1"))
merged_data = merged_data %>%
  mutate(value = ifelse(is.na(value_percentile), NA, 
    ifelse(value_percentile <= 0.3, "0",
    ifelse(value_percentile > 0.3 & value_percentile <= 0.7, "1", "2"))))

#Creating 6 portfolios
small_growth = subset(merged_data, size == "0" & value == "0")
small_neutral = subset(merged_data, size == "0" & value == "1")
small_value = subset(merged_data, size == "0" & value == "2")
large_growth = subset(merged_data, size == "1" & value == "0")
large_neutral = subset(merged_data, size == "1" & value == "1")
large_value = subset(merged_data, size == "1" & value == "2")

#Price drifting and portfolio return
#Small_growth
small_growth = setDT(small_growth)
new_small_growth = rebalance(small_growth, 7)
new_small_growth_filtered = new_small_growth[complete.cases(new_small_growth$wt),]
small_growth_portfolio_return = new_small_growth_filtered[,.(sgRet = weighted.mean(ret, wt)), by = Date]
setorder(small_growth_portfolio_return, "Date")

#Small_neutral
small_neutral = setDT(small_neutral)
new_small_neutral = rebalance(small_neutral, 7)
new_small_neutral_filtered = new_small_neutral[complete.cases(new_small_neutral$wt),]
small_neutral_portfolio_return = new_small_neutral_filtered[,.(snRet = weighted.mean(ret, wt)), by = Date]
setorder(small_neutral_portfolio_return, "Date")

#Small_value
small_value = setDT(small_value)
new_small_value = rebalance(small_value, 7)
new_small_value_filtered = new_small_value[complete.cases(new_small_value$wt),]
small_value_portfolio_return = new_small_value_filtered[,.(svRet = weighted.mean(ret, wt)), by = Date]
setorder(small_value_portfolio_return, "Date")

#Large_growth
large_growth = setDT(large_growth)
new_large_growth = rebalance(large_growth, 7)
new_large_growth_filtered = new_large_growth[complete.cases(new_large_growth$wt),]
large_growth_portfolio_return = new_large_growth_filtered[,.(lgRet = weighted.mean(ret, wt)), by = Date]
setorder(large_growth_portfolio_return, "Date")

#Large_neutral
large_neutral = setDT(large_neutral)
new_large_neutral = rebalance(large_neutral, 7)
new_large_neutral_filtered = new_large_neutral[complete.cases(new_large_neutral$wt),]
large_neutral_portfolio_return = new_large_neutral_filtered[,.(lnRet = weighted.mean(ret, wt)), by = Date]
setorder(large_neutral_portfolio_return, "Date")

#Large_value
large_value = setDT(large_value)
new_large_value = rebalance(large_value, 7)
new_large_value_filtered = new_large_value[complete.cases(new_large_value$wt),]
large_value_portfolio_return = new_large_value_filtered[,.(lvRet = weighted.mean(ret, wt)), by = Date]
setorder(large_value_portfolio_return, "Date")

#Merging portfolio returns 
data_tables = list(small_growth_portfolio_return, 
                   small_neutral_portfolio_return,
                   small_value_portfolio_return,
                   large_growth_portfolio_return, 
                   large_neutral_portfolio_return,
                   large_value_portfolio_return)
lapply(data_tables, setkey, "Date")
merged_portfolio = small_growth_portfolio_return[
    small_neutral_portfolio_return][
    small_value_portfolio_return][
    large_growth_portfolio_return][
    large_neutral_portfolio_return][large_value_portfolio_return]

#Calculating SMB and HML
merged_portfolio[, small_portfolio_return := (sgRet+snRet+svRet)/3]
merged_portfolio[, large_portfolio_return := (lgRet+lnRet+lvRet)/3]
merged_portfolio[, SMB := small_portfolio_return - large_portfolio_return]

merged_portfolio[, high_portfolio_return := (svRet+lvRet)/2]
merged_portfolio[, low_portfolio_return := (sgRet+lgRet)/2]
merged_portfolio[, HML := high_portfolio_return - low_portfolio_return]

#The final result
setkey(merged_portfolio, Date)
setkey(portfolio_return, Date)
FF3_Re = portfolio_return[merged_portfolio, .(Date, MKT, SMB, HML)]                   

#merged_data size and value
june_data = merged_data[month(Date) == 6]
june_data[,.(Date, id, ret, meTotal, book, wt)]

june_data$meTotal = na.locf(june_data$meTotal)
june_data$book = na.locf(june_data$book, fromLast = TRUE)
june_data = june_data %>%
  group_by(Date) %>%
  mutate(size_percentile = percent_rank(meTotal)) %>%
  ungroup()
june_data$beme = june_data$book/june_data$meTotal
june_data = june_data %>%
  group_by(Date) %>%
  mutate(value_percentile = percent_rank(beme)) %>%
  ungroup()

merged_data = merge(merged_data, june_data[, c("id", "Date", "size_percentile", "value_percentile")], 
                     by = c("id", "Date"), 
                     all.x = TRUE)

merged_data$size_percentile = na.locf(merged_data$size_percentile, na.rm = FALSE)
merged_data$value_percentile = na.locf(merged_data$size_percentile, na.rm = FALSE)
merged_data$size_percentile = na.locf(merged_data$size_percentile, fromLast = TRUE)
merged_data$value_percentile = na.locf(merged_data$size_percentile, fromLast = TRUE)

Industrial_production = as.data.table(read_excel("Industrial Production.xls"))
Industrial_production[, observation_date := as.Date(observation_date)]
Industrial_production = Industrial_production %>%
  mutate(MP = log(INDPRO) - log(lag(INDPRO, 1)))

Inflation = as.data.table(read_excel("CPI.xls"))
Inflation[, observation_date := as.Date(observation_date)]
Expected_Inflation = as.data.table(read_excel("1Year Expected Inflation.xls"))
Expected_Inflation[, observation_date := as.Date(observation_date)]
CPI_data = merge(Inflation, Expected_Inflation, by = "observation_date")
CPI_data = CPI_data %>%
  filter(CPI > 0) %>%
  mutate(UI = log(CPI) - lag(EI, 1))

baa_return = as.data.table(read_excel("BAA Return.xls"))
lgb = as.data.table(read_excel("Long-term government bond return.xls"))
UPR_data = merge(baa_return, lgb, by = "observation_date")
UPR_data = UPR_data %>%
  mutate(UPR = DBAA - LGB)

TRB_data = as.data.table(read_excel("4-week TB rate.xls"))
UTS_data = merge(lgb, TRB_data, by = "observation_date")
UTS_data = UTS_data %>%
  mutate(UTS = LGB - lag(DTB4WK, 1))

consumption = as.data.table(read_excel("Consumption per capita.xls"))
oil_price = as.data.table(read_excel("Petroleum Price Index.xls"))

start_date = as.Date("1986-08-01")
end_date = as.Date("2023-06-01")
Industrial_production_f = Industrial_production[observation_date >= start_date & observation_date <= end_date]
FF3_2 = FF3_Re
FF3_Re_2$Date = as.yearmon(FF3_Re_2$Date)
Industrial_production_f$observation_date = as.yearmon(Industrial_production_f$observation_date)
merged_MP = merge(FF3_Re_2, Industrial_production_f, by.x = "Date", by.y = "observation_date")
lm1 = lm(MKT ~ MP, data = merged_MP)
lm2 = lm(SMB ~ MP, data = merged_MP)
lm3 = lm(HML ~ MP, data = merged_MP)

start_date_2 = as.Date("1986-07-31")
end_date_2 = as.Date("2019-05-31")
FF3_3= FF3_Re
FF3_3_f = FF3_3[Date >= start_date_2 & Date <= end_date_2]
FF3_3_f$Date = as.yearmon(FF3_3_f$Date)
CPI_data$observation_date = as.yearmon(CPI_data$observation_date)
merged_CPI = merge(FF3_3_f, CPI_data, by.x = "Date", by.y = "observation_date")
lm4 = lm(MKT ~ UI, data = merged_CPI)
lm5 = lm(SMB ~ UI, data = merged_CPI)
lm6 = lm(HML ~ UI, data = merged_CPI)

end_date_3 = as.Date("2013-03-31")
FF3_4= FF3_Re
FF3_4_f = FF3_4[Date >= start_date_2 & Date <= end_date_3]
FF3_4_f$Date = as.yearmon(FF3_4_f$Date)
UPR_data$observation_date = as.yearmon(UPR_data$observation_date)
merged_UPR = merge(FF3_4_f, UPR_data, by.x = "Date", by.y = "observation_date")
lm7 = lm(MKT ~ UPR, data = merged_UPR)
lm8 = lm(SMB ~ UPR, data = merged_UPR)
lm9 = lm(HML ~ UPR, data = merged_UPR)

start_date_3 = as.Date("2018-09-30")
end_date_4 = as.Date("2022-02-28")
FF3_5= FF3_Re
FF3_5_f = FF3_5[Date >= start_date_3 & Date <= end_date_4]
FF3_5_f$Date = as.yearmon(FF3_5_f$Date)
UTS_data$observation_date = as.yearmon(UTS_data$observation_date)
merged_UTS = merge(FF3_5_f, UTS_data, by.x = "Date", by.y = "observation_date")
lm10 = lm(MKT ~ UTS, data = merged_UTS)
lm11 = lm(SMB ~ UTS, data = merged_UTS)
lm12 = lm(HML ~ UTS, data = merged_UTS)

