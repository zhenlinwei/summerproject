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

##################################################
ff3_replicated = as.data.table(read.csv("FF3 replicated.csv"))
industrial_production = as.data.table(read_excel("Industrial Production.xls"))
Oneyear_inflation = as.data.table(read_excel("1Year Expected Inflation.xls"))
CPI = as.data.table(read_excel("CPI.xls"))
longterm_bond = as.data.table(read_excel("Long-term government bond return.xls"))
oil_price = as.data.table(read_excel("Petroleum Price Index.xls"))
baa = as.data.table(read_excel("BAA.xls"))
Tbill = as.data.table(read_excel("Tbill.xls"))
consumption = as.data.table(read_excel("Consumption.xlsx"))

industrial_production$MP = log(industrial_production$INDPRO/lag(industrial_production$INDPRO, 1))
unexpected_inflation = merge(Oneyear_inflation, CPI, by = "observation_date", all = TRUE)
unexpected_inflation$UI = log(unexpected_inflation$CPI) - lag(unexpected_inflation$EI, 1)
risk_premia = merge(baa, longterm_bond, by = "observation_date", all = TRUE)
risk_premia$UPR = risk_premia$BAA - risk_premia$LGB
term_structure = merge(longterm_bond, Tbill, by = "observation_date", all = TRUE)
term_structure$UTS = term_structure$LGB - term_structure$GS1M


ff3_replicated$Date = as.Date(ff3_replicated$Date)
ff3_replicated$year = year(ff3_replicated$Date)
ff3_replicated$month = month(ff3_replicated$Date)

industrial_production$MP = lag(industrial_production$MP)
industrial_production$year = year(industrial_production$observation_date)
industrial_production$month = month(industrial_production$observation_date)
ff3_replicated = merge(ff3_replicated, industrial_production[, c("year", "month", "MP")], by = c("year", "month"), all.x = TRUE)
unexpected_inflation$UI = lag(unexpected_inflation$UI)
unexpected_inflation$year = year(unexpected_inflation$observation_date)
unexpected_inflation$month = month(unexpected_inflation$observation_date)
ff3_replicated = merge(ff3_replicated, unexpected_inflation[, c("year", "month", "UI")], by = c("year", "month"), all.x = TRUE)
risk_premia$UPR = lag(risk_premia$UPR)
risk_premia$year = year(risk_premia$observation_date)
risk_premia$month = month(risk_premia$observation_date)

ff3_replicated = merge(ff3_replicated, risk_premia[, c("year", "month", "UPR")], by = c("year", "month"), all.x = TRUE)
term_structure$UTS = lag(term_structure$UTS)
term_structure$year = year(term_structure$observation_date)
term_structure$month = month(term_structure$observation_date)
ff3_replicated = merge(ff3_replicated, term_structure[, c("year", "month", "UTS")], by = c("year", "month"), all.x = TRUE)
consumption$con = lag(consumption$con)
consumption$year = year(consumption$Date)
consumption$month = month(consumption$Date)
ff3_replicated = merge(ff3_replicated, consumption[, c("year", "month", "con")], by = c("year", "month"), all.x = TRUE)
oil_price$oilprice = lag(oil_price$oilprice)
oil_price$year = year(oil_price$observation_date)
oil_price$month = month(oil_price$observation_date)
ff3_replicated = merge(ff3_replicated, oil_price[, c("year", "month", "oilprice")], by = c("year", "month"), all.x = TRUE)

mktlm = lm(MKT~ MP+UI+UPR+UTS+con+oilprice, data = FF3_Re)
smblm = lm(SMB~ MP+UI+UPR+UTS+con+oilprice, data = FF3_Re)
hmllm = lm(HML~ MP+UI+UPR+UTS+con+oilprice, data = FF3_Re)

summary(mktlm)
summary(smblm)
summary(hmllm)


subset_01 = ff3_replicated[ff3_replicated$year >= 2001, ]
mktlm01 = lm(MKT~ MP+UI+UPR+UTS+con+oilprice, data = subset_01)
smblm01 = lm(SMB~ MP+UI+UPR+UTS+con+oilprice, data = subset_01)
hmllm01 = lm(HML~ MP+UI+UPR+UTS+con+oilprice, data = subset_01)

summary(mktlm01)
summary(smblm01)
summary(hmllm01)

library(ggplot2)
subset_01$cumulative_smb = cumprod(1 + subset_01$SMB)-1
subset_01$cumulative_mkt = cumprod(1 + subset_01$MKT)-1

p = ggplot(subset_01, aes(x = Date)) +
  geom_line(aes(y = cumulative_mkt, color = "MKT")) +
  geom_line(aes(y = UI, color = "UI")) +
  scale_color_manual(values = c("MKT" = "blue", "UI" = "red")) +
  scale_y_continuous(name = "MKT", 
                     sec.axis = sec_axis(~./1, name = "UI")) +
  labs(x = "Time", color = "Variable") +
  theme_minimal()

print(p)

p2 = ggplot(subset_01, aes(x = Date)) +
  geom_line(aes(y = cumulative_smb, color = "smb")) +
  geom_line(aes(y = oilprice, color = "oilprice")) +
  scale_color_manual(values = c("smb" = "blue", "oilprice" = "red")) +
  scale_y_continuous(name = "smb", 
                     sec.axis = sec_axis(~./1, name = "oilprice")) +
  labs(x = "Time", color = "Variable") +
  theme_minimal()

print(p2)
                   
