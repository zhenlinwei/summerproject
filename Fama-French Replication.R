file1 = "financial data.csv"
financial = read.csv(file1)
file2 = "market data.csv"
market = read.csv(file2)
market$Date = as.Date(market$Date)
financial$annDate = as.Date(financial$annDate)
mergedata = merge(market,financial, by = "id", all.x = TRUE, roll = "nearest")
merged = mergedata[order(mergedata$Date),]