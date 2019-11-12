library(data.table)

files <- list.files("/tmp/foo")

allFiles <- data.table()
for (file in files) {
  thisFile <- fread(paste0("/tmp/foo/",file))
  thisFile[, symbol := NULL]
  coinName <- gsub('.{9}$', '', file)
  thisFile$symbol <- coinName
  allFiles <- rbind(allFiles, thisFile)  
}

additional <- allFiles[, list(symbol, timestamp, pumplikelihood)]

additionalStats <- getAllStats(head(additional[order(timestamp)],50), 60)
addmcapstats <- merge(additionalStats, CMCsupply)
addmcapstats[, marketCap := initialPrice * circulating_supply]
print(paste("There are", nrow(addmcapstats[initialPrice == 0]), "pumps that are really just new price listings. We omit them."))
addmcapstats <- addmcapstats[initialPrice > 0]
marketCapsPlot <- ggplot(addmcapstats[pumplikelihood > 0.9]) +
  geom_histogram(aes(x=marketCap/10^6), color="white", binwidth = 10) +
  theme_Publication() +
  labs(x = "Cryptocurrency market capitalization in million $", y="Number of pumps")


additional[, time:= as.POSIXct(timestamp, origin="1970-01-01")]
topCoinsPlot <- ggplot(additional[symbol %in% additional[, list(count = .N), by=symbol][count >= 10]$symbol]) + geom_point(aes(x=time, y=symbol, color="found"), size=3) +
  geom_point(data = pumps[symbol %in% additional[, list(count = .N), by=symbol][count >= 10]$symbol], aes(x=time, y=symbol, color="known")) +
  theme_Publication() + labs(x="Time", y="Symbol")
ggsave(filename = "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/images/topCoins.pdf", plot = topCoinsPlot, width = 6, height = 5)
