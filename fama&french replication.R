library(readxl)
library(dplyr)
library(zoo)
library(data.table)
library(lubridate)
setwd("C:/Users/HarryZhao/OneDrive - Rayliant Global Advisors/Rayliant/Student Program/2023 Summer/Zhenlin/summerproject/")
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

# ff3_replicated = as.data.table(read.csv("FF3 replicated.csv"))
fama = as.data.table(read.csv("fama.csv"), stringsAsFactors=FALSE)
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
unexpected_inflation$UI = unexpected_inflation$CPI - lag(unexpected_inflation$EI, 1)
risk_premia = merge(baa, longterm_bond, by = "observation_date", all = TRUE)
risk_premia$URP = risk_premia$BAA - risk_premia$LGB
term_structure = merge(longterm_bond, Tbill, by = "observation_date", all = TRUE)
term_structure$UTS = term_structure$LGB - term_structure$GS1M

ff3_replicated <- fama[,.(Date = as.yearmon(paste0(substr(Date, 1, 4), "-", substr(Date, 5, 6))),
                          year = as.numeric(substr(Date, 1, 4)), month = as.numeric(substr(Date, 5, 6)), MKT = as.numeric(Mkt.RF)/100 + as.numeric(RF)/100, 
                          SMB = as.numeric(SMB)/100, HML= as.numeric(HML)/100, RF = as.numeric(RF)/100)]

# ff3_replicated$Date = as.Date(ff3_replicated$Date)
# ff3_replicated$year = year(ff3_replicated$Date)
# ff3_replicated$month = month(ff3_replicated$Date)


industrial_production$MP = lag(industrial_production$MP)
industrial_production$year = year(industrial_production$observation_date)
industrial_production$month = month(industrial_production$observation_date)
ff3_replicated = merge(ff3_replicated, industrial_production[, c("year", "month", "MP")], by = c("year", "month"), all.x = TRUE)
unexpected_inflation$UI = lag(unexpected_inflation$UI)
unexpected_inflation$year = year(unexpected_inflation$observation_date)
unexpected_inflation$month = month(unexpected_inflation$observation_date)
ff3_replicated = merge(ff3_replicated, unexpected_inflation[, c("year", "month", "UI")], by = c("year", "month"), all.x = TRUE)
risk_premia$URP = lag(risk_premia$URP)
risk_premia$year = year(risk_premia$observation_date)
risk_premia$month = month(risk_premia$observation_date)

ff3_replicated = merge(ff3_replicated, risk_premia[, c("year", "month", "URP")], by = c("year", "month"), all.x = TRUE)
term_structure$UTS = lag(term_structure$UTS)
term_structure$year = year(term_structure$observation_date)
term_structure$month = month(term_structure$observation_date)
ff3_replicated = merge(ff3_replicated, term_structure[, c("year", "month", "UTS")], by = c("year", "month"), all.x = TRUE)
consumption$con = lag(consumption$con)
consumption[, con_growth := con/lag(con)-1]
consumption$year = year(consumption$Date)
consumption$month = month(consumption$Date)

ff3_replicated = merge(ff3_replicated, consumption[, c("year", "month", "con", "con_growth")], by = c("year", "month"), all.x = TRUE)

oil_price$oilprice = lag(oil_price$oilprice)
oil_price[, oil_growth := oilprice/lag(oilprice)-1]
oil_price$year = year(oil_price$observation_date)
oil_price$month = month(oil_price$observation_date)
ff3_replicated = merge(ff3_replicated, oil_price[, c("year", "month", "oil_growth", "oilprice")], by = c("year", "month"), all.x = TRUE)


# summary(mktlm)
# summary(smblm)
# summary(hmllm)


subset_01 = ff3_replicated[ff3_replicated$year >= 1999, ]

library(ggplot2)

subset_01$cumulative_smb = cumprod(1 + subset_01$SMB)-1
subset_01$cumulative_mkt = cumprod(1 + subset_01$MKT)-1
subset_01$cumulative_hml = cumprod(1 + subset_01$HML)-1

# p = ggplot(subset_01, aes(x = Date)) +
#   geom_line(aes(y = cumulative_mkt, color = "MKT")) +
#   geom_line(aes(y = UI, color = "UI")) +
#   scale_color_manual(values = c("MKT" = "blue", "UI" = "red")) +
#   scale_y_continuous(name = "MKT", 
#                      sec.axis = sec_axis(~./1, name = "UI")) +
#   labs(x = "Time", color = "Variable") +
#   theme_minimal()
# 
# print(p)
# 
p2 = ggplot(subset_01, aes(x = Date)) +
  geom_line(aes(y = cumulative_smb, color = "smb")) +
  geom_line(aes(y = oilprice/100, color = "oilprice")) +
  scale_color_manual(values = c("smb" = "blue", "oilprice" = "red")) +
  scale_y_continuous(name = "smb",
                     limits = c(0,5),
                     sec.axis = sec_axis(~./1, name = "oilprice")) +
  labs(x = "Time", color = "Variable") +
  theme_minimal()

print(p2)

library(dplyr)

# subset_01 = subset_01 %>%
#   mutate(signal = sign(oil_growth),
#          trading_signal = case_when(
#            signal == 1 & lag(signal) == 1 ~ "1",
#            signal == -1 & lag(signal) == -1 ~ "-1",
#            TRUE ~ "0"
#          ))
# subset_01 = subset_01 %>%
#   select(-signal)
# fama_subset <- fama[871:1161, .(RF)] 
# if(nrow(fama_subset) > nrow(subset_01)) {
#   fama_subset = fama_subset[1:nrow(subset_01), ]
# } else {
#   subset_01 = subset_01[1:nrow(fama_subset), ]
# }
# subset_01 = cbind(subset_01, fama_subset)
# 
# subset_01 = subset_01 %>%
#   mutate(trading_signal = replace(trading_signal, trading_signal == 0, NA)) %>%
#   fill(trading_signal, .direction = "down")
# 
# subset_01$trading_signal[is.na(subset_01$trading_signal)]<- -1
# subset_01$SMB = as.numeric(subset_01$SMB)
# subset_01$RF = as.numeric(subset_01$RF)
# subset_01 = subset_01 %>%
#   mutate(ret = case_when(
#     trading_signal == 1 ~ SMB,
#     trading_signal == -1 ~ RF/100,
#     TRUE ~ NA_real_
#   ))
# subset_01 = subset_01 %>%
#   fill(ret, .direction = "up")
# subset_01$cumulative_ret = cumprod(1 + subset_01$ret)-1
# p3 = ggplot(subset_01, aes(x = Date)) +
#   geom_line(aes(y = cumulative_smb, color = "smb")) +
#   geom_line(aes(y = cumulative_ret, color = "ret")) +
#   scale_color_manual(values = c("smb" = "blue", "ret" = "red")) +
#   scale_y_continuous(name = "smb",
#                      sec.axis = sec_axis(~./1, name = "ret")) +
#   labs(x = "Time", color = "Variable") +
#   theme_minimal()
# 
# print(p3)
# 
# subset_01_1 = subset_01[, .(Date, trading_signal, SMB, RF, ret)]
# subset_01_1 = subset_01_1[-1, ]
# prod(subset_01_1$ret+1)-1
# prod(subset_01_1$SMB+1)-1
 
#consumption and oil price growth to regression 
mktlm01 = lm(MKT~ MP+UI+URP+UTS+con_growth+oil_growth, data = subset_01)
smblm01 = lm(SMB~ MP+UI+URP+UTS+con_growth+oil_growth, data = subset_01)
hmllm01 = lm(HML~ MP+UI+URP+UTS+con_growth+oil_growth, data = subset_01)

summary(mktlm01)
summary(smblm01)
summary(hmllm01)

#MKT vs UI graph
# print(p)

#rolling look back, 12 month rolling average of UI if +, long mkt, if -, short mkt->rf

subset_01$rolling_avg = rollmean(subset_01$UI, k=12, align="right", fill=NA)
subset_01$mkt_sig = ifelse(subset_01$UI > subset_01$rolling_avg, 1, 0)
subset_01$mkt_trading = ifelse(subset_01$UI > subset_01$rolling_avg, subset_01$MKT, subset_01$RF)

# subset_01$oil_avg = rollmean(subset_01$oilprice, k=6, align="right", fill=NA)
subset_01$smb_sig = ifelse(subset_01$oilprice > lag(subset_01$oilprice), 1, 0)
subset_01$smb_trading = ifelse(subset_01$oilprice > lag(subset_01$oilprice), subset_01$SMB, -subset_01$SMB)

# subset_01$uts_avg = rollmean(subset_01$UTS, k=12, align="right", fill=NA)
subset_01$hml_sig1 = ifelse(subset_01$UTS > lag(subset_01$UTS), 1, 0)
subset_01$hml_trading1 = ifelse(subset_01$UTS > lag(subset_01$UTS), subset_01$HML, -subset_01$HML)

# subset_01$con_avg = rollmean(subset_01$con_growth, k=12, align="right", fill=NA)
subset_01$hml_sig2 = ifelse(subset_01$con > lag(subset_01$con), 1, 0)
subset_01$hml_trading2 = ifelse(subset_01$con > lag(subset_01$con), subset_01$HML, -subset_01$HML)

tradingStrats = subset_01[-(1:12), ]

# tradingStrats$cumulative_hml = cumprod(1 + tradingStrats$HML)-1
tradingStrats$mkt_ui = cumprod(1 + tradingStrats$mkt_trading)-1
tradingStrats$smb_oil = cumprod(1 + tradingStrats$smb_trading)-1
tradingStrats$hml_uts = cumprod(1 + tradingStrats$hml_trading1)-1
tradingStrats$hml_con = cumprod(1 + tradingStrats$hml_trading2)-1

# # check
# ggplot(subset_01, aes(x = Date)) +
#   geom_line(aes(y = cumulative_mkt, color = "cumulative_smb")) +
#   geom_line(aes(y = UI/1, color = "UI")) +
#   scale_color_manual(values = c("cumulative_smb" = "blue", "UI" = "red")) +
#   scale_y_continuous(name = "cumulative_smb",
#                      sec.axis = sec_axis(~./1, name = "")) +
#   labs(x = "Time", color = "Variable") +
#   theme_minimal()

ggplot(tradingStrats, aes(x = Date)) +
  geom_line(aes(y = cumulative_mkt, color = "cumulative_smb")) +
  geom_line(aes(y = mkt_ui, color = "mkt_ui")) +
  scale_color_manual(values = c("cumulative_smb" = "blue", "mkt_ui" = "red")) +
  scale_y_continuous(name = "cumulative_smb",
                     sec.axis = sec_axis(~./1, name = "")) +
  labs(x = "Time", color = "Variable") +
  theme_minimal()

ggplot(tradingStrats, aes(x = Date)) +
  geom_line(aes(y = cumulative_smb, color = "cumulative_smb")) +
  geom_line(aes(y = smb_oil, color = "smb_oil")) +
  scale_color_manual(values = c("cumulative_smb" = "blue", "smb_oil" = "red")) +
  scale_y_continuous(name = "cumulative_smb",
                     sec.axis = sec_axis(~./1, name = "")) +
  labs(x = "Time", color = "Variable") +
  theme_minimal()

ggplot(tradingStrats, aes(x = Date)) +
  geom_line(aes(y = cumulative_hml, color = "cumulative_hml")) +
  geom_line(aes(y = hml_uts, color = "hml_uts")) +
  geom_line(aes(y = URP/10, color = "hml_uts")) +
  scale_color_manual(values = c("cumulative_hml" = "blue", "hml_uts" = "red")) +
  scale_y_continuous(name = "cumulative_hml",
                     sec.axis = sec_axis(~./10, name = "urp")) +
  labs(x = "Time", color = "Variable") +
  theme_minimal()

ggplot(tradingStrats, aes(x = Date)) +
  geom_line(aes(y = cumulative_hml, color = "cumulative_hml")) +
  geom_line(aes(y = hml_con, color = "hml_con")) +
  scale_color_manual(values = c("cumulative_hml" = "blue", "hml_con" = "red")) +
  scale_y_continuous(name = "cumulative_hml",
                     sec.axis = sec_axis(~./1, name = "cumulative_con_growth")) +
  labs(x = "Time", color = "Variable") +
  theme_minimal()



# -----------------

# 
# 
# signal_return = subset_01[, .(Date, MKT, HML, SMB, RF, con_growth, oil_growth, oil_avg, con_avg, cumulative_smb, cumulative_hml)]
# signal_return$hml_trading = ifelse(signal_return$con_growth > signal_return$con_avg, "1", "-1")
# signal_return$smb_trading = ifelse(signal_return$oil_growth > signal_return$oil_avg, "1", "-1")
# 
# signal_return = signal_return %>%
#   mutate(smb_hml_rf = case_when(
#     hml_trading == 1 & smb_trading == -1 ~ HML,
#     hml_trading == -1 & smb_trading == 1 ~ SMB,
#     hml_trading == 1 & smb_trading == 1 ~ 0.5*SMB + 0.5*HML,
#     hml_trading == -1 & smb_trading == -1 ~ RF/100,
#   ))
# 
# signal_return$cumulative_ret = cumprod(1 + signal_return$ret)-1
# 
# p7 = ggplot(signal_return, aes(x = Date)) +
#   geom_line(aes(y = cumulative_smb, color = "cumulative_smb")) +
#   geom_line(aes(y = cumulative_ret, color = "cumulative_ret")) +
#   scale_color_manual(values = c("cumulative_smb" = "blue", "cumulative_ret" = "red")) +
#   scale_y_continuous(name = "cumulative_smb",
#                      sec.axis = sec_axis(~./1, name = "cumulative_ret")) +
#   labs(x = "Time", color = "Variable") +
#   theme_minimal()
# 
# print(p7)
# 
# p8 = ggplot(signal_return, aes(x = Date)) +
#   geom_line(aes(y = cumulative_hml, color = "cumulative_hml")) +
#   geom_line(aes(y = cumulative_ret, color = "cumulative_ret")) +
#   scale_color_manual(values = c("cumulative_hml" = "blue", "cumulative_ret" = "red")) +
#   scale_y_continuous(name = "cumulative_hml",
#                      sec.axis = sec_axis(~./1, name = "cumulative_ret")) +
#   labs(x = "Time", color = "Variable") +
#   theme_minimal()
# 
# print(p8)
# 
# 
# signal_return$hml_rf = ifelse(signal_return$con_growth > signal_return$con_avg, signal_return$HML, signal_return$RF/100)
# signal_return$smb_rf = ifelse(signal_return$oil_growth > signal_return$oil_avg, signal_return$SMB, signal_return$RF/100)

portfolio = tradingStrats[, .(Date, MKT, SMB, HML, RF, smb_trading, hml_trading2)]
portfolio[, portRet0 := 0.5 * smb_trading + 0.5 * hml_trading2]
portfolio[, portRet1 := 0.25 * smb_trading + 0.25 * hml_trading2 + 0.5 * MKT]
portfolio[, portRet2 := 0.25 * SMB + 0.25 * HML + 0.5 * MKT]
cumPort <- portfolio[,.(Date, 
                        mktCum = cumprod(1+MKT)-1, 
                        smbCum = cumprod(1+SMB)-1, 
                        hmlCum = cumprod(1+HML)-1,
                        smbStrCum = cumprod(1+smb_trading)-1,
                        hmlStrCum = cumprod(1+hml_trading2)-1,
                        retCum0 = cumprod(1+portRet0)-1,
                        retCum1 = cumprod(1+portRet1)-1,
                        retCum2 = cumprod(1+portRet2)-1)]

ggplot(cumPort, aes(x = Date)) +
  geom_line(aes(y = mktCum, color = "mktCum")) +
  # geom_line(aes(y = smbStrCum, color = "smbStrCum")) +
  # geom_line(aes(y = hmlStrCum, color = "hmlStrCum")) +
  geom_line(aes(y = retCum0, color = "retCum0")) +
  geom_line(aes(y = retCum1, color = "retCum1")) +
  geom_line(aes(y = retCum2, color = "retCum2")) 

# strategy analytics
drawdown <- function(pnl) {
  cumPnl  <- cumprod(1+pnl)
  drawdown <- cumPnl/cummax(cumPnl)-1
  return(tail(drawdown, -1))
}

maxdrawdown <- function(pnl)min(drawdown(pnl))

getAnalytics <- function(ret){
  annRet = prod(1+ret)^(12/nrow(portfolio))-1
  annVol = sd(ret)*sqrt(12)
  sr = (prod(1+ret- portfolio$RF)^(12/nrow(portfolio))-1)/(sd(ret)*sqrt(12))
  maxDD = maxdrawdown(ret)
  return(c("Ann. Return" = annRet, 
           "Ann. Vol." = annVol, 
           "Sharpe Ratio" = sr, 
           "Max Drawdown" = maxDD))
}

lapply(portfolio[,.(MKT, SMB, HML, portRet1)], getAnalytics)

# mkt_ann_ret = prod(1+portfolio$MKT)^(12/267)-1
# smb_ann_ret = prod(1+portfolio$SMB)^(12/267)-1
# hml_ann_ret = prod(1+portfolio$HML)^(12/267)-1
# smbrf_ann_ret = prod(1+portfolio$smb_rf)^(12/267)-1
# hmlrf_ann_ret = prod(1+portfolio$hml_rf)^(12/267)-1
# smbhmlrf_ann_ret = prod(1+portfolio$smb_hml_rf)^(12/267)-1
# 
# mkt_ann_vol = sd(portfolio$MKT)*sqrt(12)
# smb_ann_vol = sd(portfolio$SMB)*sqrt(12)
# hml_ann_vol = sd(portfolio$hml)*sqrt(12)
# smbrf_ann_vol = sd(portfolio$smb_rf)*sqrt(12)
# hmlrf_ann_vol = sd(portfolio$hml_rf)*sqrt(12)
# smbhmlrf_ann_vol = sd(portfolio$smb_hml_rf)*sqrt(12)
# 
# portfolio$RF = portfolio$RF/100
# portfolio$mktnum = portfolio$MKT - portfolio$RF
# portfolio$smbnum = portfolio$SMB - portfolio$RF
# portfolio$hmlnum = portfolio$HML - portfolio$RF
# portfolio$smbrfnum = portfolio$smb_rf - portfolio$RF
# portfolio$hmlrfnum = portfolio$hml_rf - portfolio$RF
# portfolio$smbhmlrfnum = portfolio$smb_hml_rf - portfolio$RF
# 
# ann_mktnum = prod(1+portfolio$mktnum)^(12/267)-1
# ann_smbnum = prod(1+portfolio$smbnum)^(12/267)-1
# ann_hmlnum = prod(1+portfolio$hmlnum)^(12/267)-1
# ann_smbrfnum = prod(1+portfolio$smbrfnum)^(12/267)-1
# ann_hmlrfnum = prod(1+portfolio$hmlrfnum)^(12/267)-1
# ann_smbhmlrfnum = prod(1+portfolio$smbhmlrfnum)^(12/267)-1
# 
# mkt_sharpe = ann_mktnum/mkt_ann_vol
# smb_sharpe = ann_smbnum/smb_ann_vol
# hml_sharpe = ann_hmlnum/hml_ann_vol
# smbrf_sharpe = ann_smbrfnum/smbrf_ann_vol
# hmlrf_sharpe = ann_hmlrfnum/hmlrf_ann_vol
# smbhmlrf_sharpe = ann_smbhmlrfnum/smbhmlrf_ann_vol
# 
# values = c(mkt_sharpe, smb_sharpe, hml_sharpe, smbrf_sharpe, hmlrf_sharpe, smbhmlrf_sharpe)
# names = c("mkt_sharpe", "smb_sharpe", "hml_sharpe", "smbrf_sharpe", "hmlrf_sharpe", "smbhmlrf_sharpe")
# 
# portfolio$cumulative_mkt = cumprod(1 + portfolio$hmlrfnum)-1
# 
# ggplot(portfolio, aes(x=Date, y=cumulative_mkt)) +
#   geom_line() +
#   labs(title="HML over Time", x="Date", y="HML") +
#   theme_minimal()
                   
