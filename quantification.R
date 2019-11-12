library(data.table)
library(ggplot2)
library(scales)
library(RSQLite)
library(RPostgreSQL)
library(dbscan)
source("publication_theme.R")

CMCcoins <- jsonlite::fromJSON("https://pro-api.coinmarketcap.com/v1/cryptocurrency/listings/latest?limit=5000&CMC_PRO_API_KEY=70fc3afd-8d17-40ed-95ec-2cf9a74bd02e")$data
CMCcoins$quote <- do.call(rbind, lapply(CMCcoins[c('quote')], '[[', 'USD'))$price
CMCcoins <- as.data.table(CMCcoins)

psqlCon <- dbConnect(drv = dbDriver("PostgreSQL"), dbname = "tickdata",
                     host = "localhost", port = 54321,
                     user = "postgres", password = "DifficultPassword1")

con = dbConnect(drv=RSQLite::SQLite(), dbname="telegramPnD.sqlite")
pumps <- dbGetQuery(con, "SELECT * FROM flags LEFT JOIN messages ON flags.channel_id = messages.channel_id AND flags.message_id = messages.message_id WHERE type='pump' AND exchange = 'Binance' AND timestamp > 1530804991;") #  AND timestamp < 1540613716
pumps <- as.data.table(pumps)
pumps[, cluster := dbscan(pumps[,list(timestamp, as.numeric(as.factor(symbol))*100)], eps=5*60, minPts = 1)$cluster]
pumps <- pumps[, list(symbol=unique(symbol),
                      type = unique(type),
                      timestamp = min(timestamp),
                      maxTimeDev = max(timestamp) - min(timestamp),
                      channelCount = length(channel_id)),
               by=cluster]
# correct timestamp by shifting
pumps[, timestamp := timestamp + 2*60*60]
# account for winter time
pumps[timestamp > 1540684799 & timestamp < 1553990400, timestamp := timestamp-(60*60)]
pumps[, time := as.POSIXct(timestamp, origin="1970-01-01")]


pumpMessageViews <- dbGetQuery(con, "SELECT * FROM flags
LEFT JOIN messages ON flags.channel_id = messages.channel_id AND flags.message_id = messages.message_id
LEFT JOIN messageviews ON flags.channel_id = messageviews.channel_id AND flags.message_id = messageviews.message_id
WHERE type='pump' AND exchange = 'Binance' AND timestamp > 1530804991 AND timestamp < 1563926400;")
pumpMessageViews <- as.data.table(pumpMessageViews)
viewStats <- pumpMessageViews[, list(delay=min(crawl_time-timestamp), views = min(views)), by=list(channel_id, message_id)]


pumps[, minuteRange := as.POSIXlt(lubridate::round_date(time, "1 minute"))$min]
pumpTimes <- ggplot(pumps, aes(x=minuteRange)) +
  geom_histogram(aes(y=..count../sum(..count..)), color="white", binwidth = 1) +
  scale_x_continuous(breaks=c(0, 30, 59)) +
  scale_y_continuous(labels=scales::percent) +
  labs(x="Minute of the hour", y="Of all pumps") +
  theme_Publication()
ggsave(filename = "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/images/pumptimes.pdf", plot = pumpTimes, width = 5.5, height = 2.2)


readPair <- function(name) {
  coin <- fread(paste0("~/workspaces/data/cryptocoins/binance/pairs/",name,".csv"))
  # trim to seconds, removing duplicates
  coin <- coin[, list(s = head(s, 1), c=head(c, 1), q = max(q), n = max(n)), by=list(loctime = as.integer(round(loctime/1000)))]
  
  # extract full timespan
  fulltimespan <- data.table(loctime=as.numeric(seq(min(coin$loctime), max(coin$loctime))))
  # extend original dataset to encompass all timestamps
  coin <- merge(coin[, list(loctime, c, q, n)], fulltimespan, by="loctime", all.y = T)
  
  # apply last observation carried forward data imputation
  f_mylocf <- function(x) {x[cummax((!is.na(x)) * seq_along(x))]}
  coin[, c("c", "q", "n") := list(f_mylocf(c), f_mylocf(q), f_mylocf(n))]
  
  # add usd volume
  quoteCurrency <- substr(name, nchar(name)-3+1, nchar(name))
  switch(quoteCurrency,
         BTC={ # should be btcusdt
           coin <- merge(coin, BTCUSDT[, list(loctime, btcprice = c)], all.x = T)
           coin <- coin[, list(loctime, cusd = c*btcprice, qusd = q*btcprice, n)]
         },
         ETH={
           print("ETH")
         },
         BNB={ # should be bnbusdt
           coin <- merge(coin, eosusdt[, list(loctime, bnbprice = c)], all.x = T)
           coin <- coin[, list(loctime, c, cusd = c*bnbprice, q, qusd = q*bnbprice, n)]
         },
         SDT={
           print("USDT")
         })
  
  # convert unix timestamps to POSIXct, with a 2 hour shift
  coin[, time := as.POSIXct(loctime + 2*60*60, origin="1970-01-01")]
  return(na.omit(coin))
}

pairToOHLC <- function(pair, size = "1 day") {
  return(as.xts.data.table(pair[, list(Open = head(c, 1), High = max(c), Low = min(c), Close = tail(c, 1)), by=list(index=as.POSIXct(cut(time, size)))]))
}

BTCUSDT <- readPair("BTCUSDT")

getPump <- function(coin, timestamp, windowSize = 60, fromDB=TRUE) {
  windowSize <- round(windowSize/2)
  if(fromDB) {
    coin <- as.data.table(dbGetQuery(psqlCon, paste0("SELECT s, c, a, b, q, n, loctime
    FROM cryptoticks
    WHERE s = '",coin,"BTC' AND
    locdatetime >= (SELECT to_timestamp(",timestamp,") - INTERVAL '",windowSize," secs') AND
    locdatetime <= (SELECT to_timestamp(",timestamp,") + INTERVAL '",windowSize," secs')
    ORDER BY locdatetime DESC;")))
    if(nrow(coin) == 0) {
      return(data.table())
    }
    btcusdt <- as.data.table(dbGetQuery(psqlCon, paste0("SELECT s, c, a, b, q, n, loctime
    FROM cryptoticks
    WHERE s = 'BTCUSDT' AND
    locdatetime >= (SELECT to_timestamp(",timestamp,") - INTERVAL '",windowSize," secs') AND
    locdatetime <= (SELECT to_timestamp(",timestamp,") + INTERVAL '",windowSize," secs')
    ORDER BY locdatetime DESC;")))
    
    # apply last observation carried forward data imputation
    f_mylocf <- function(x) {x[cummax((!is.na(x)) * seq_along(x))]}
    btcusdt[, c("c") := list(f_mylocf(c))]
    
    coin <- merge(coin, btcusdt[, list(loctime, btcprice = c)], all.x = T)
    coin[, c("c", "q", "n", "btcprice") := list(f_mylocf(c), f_mylocf(q), f_mylocf(n),  f_mylocf(btcprice))]
    coin <- coin[, list(loctime = loctime/1000, cusd = c*btcprice, qusd = q*btcprice, n, time = as.POSIXct(loctime/1000, origin="1970-01-01"))]
    
    return(coin)
  }
  else {
    if(!exists(paste0(coin,"BTC"))) {
      eval(parse(text=paste0(coin,"BTC <<- readPair('",coin,"BTC')")))
    }
    return(eval(parse(text=paste0(coin,"BTC[loctime > timestamp-windowSize & loctime < timestamp+windowSize]"))))
  }

}

extractVolBars <- function(coin) {
  coin[, volume := ifelse(n > shift(n), pmax(0, q-shift(q)),
                          ifelse(c != shift(c) & n < shift(n), pmax(0, shift(q) - ((shift(n) - n) * (q/n)) - q), 0))]
  
  coin <- coin[, list(loctime = loctime/1000, c, q, n, time = as.POSIXct(loctime/1000, origin="1970-01-01"), volume)]
  coinTicks <- coin[volume != 0]
  coinVols <- coinTicks[, list(loctime, c, cumvol = round(cumsum(volume)/0.1))][cumsum(rle(cumvol)$lengths)]
}

getPumpBTCVols <- function(coin, timestamp, windowSize = 60, fromDB=TRUE) {
  windowSize <- round(windowSize/2)
  if(fromDB) {
    coin <- as.data.table(dbGetQuery(psqlCon, paste0("SELECT s, c, a, b, q, n, loctime
    FROM cryptoticks
    WHERE s = '",coin,"BTC' AND
    locdatetime >= (SELECT to_timestamp(",timestamp,") - INTERVAL '",windowSize," secs') AND
    locdatetime <= (SELECT to_timestamp(",timestamp,") + INTERVAL '",windowSize," secs')
    ORDER BY locdatetime ASC;")))
    if(nrow(coin) == 0) {
      return(data.table())
    }
    
    coinVols <- extractVolBars(coin)
    
    return(coinVols)
  }
}

plotPump <- function(coin, timestamp, windowSize = 60, withVolume=TRUE) {
  pump <- getPump(coin, timestamp, windowSize)
  plotData <- pump[, list(time, value = cusd, type="Price in USD")]
  if(withVolume) {
    plotData <- rbind(plotData,
                      pump[, list(time, value = qusd-min(pump$qusd), type="Cum. volume in USD")])
    plotData$type <- factor(plotData$type, levels = c("Price in USD","Cum. volume in USD"))
  }
  ggplot(plotData) +
    annotation_custom(grid::textGrob(coin, gp=grid::gpar(fontsize=96, alpha=0.05)),xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    geom_line(aes(x=time, y=value)) +
    facet_grid(type ~ ., scales="free") +
    scale_y_continuous(labels = comma) +
    theme_Publication() +
    geom_vline(xintercept = as.POSIXct(timestamp, origin="1970-01-01"), linetype="dashed", color="red") +
    labs(x=paste("Time on",as.Date(as.POSIXct(timestamp, origin="1970-01-01"))), y="") +
    scale_x_continuous(trans = scales::time_trans(tz = "UTC"))
}

plotPumpFeatures <- function(coin, timestamp, windowSize = 60) {
  pump <- getPump(coin, timestamp, windowSize)
  plotData <- rbind(pump[, list(time, value = cusd, type="1) Price in USD")],
                    pump[, list(time, value = cusd/shift(cusd, 10)-1, type="2) %change_10")],
                    pump[, list(time, value = rollapply(cusd, width = 5, FUN=sd, fill=NA), type="Price SD 5")],
                    pump[, list(time, value = cusd/shift(cusd), type="Price Change 2")])
  #plotData$type <- factor(plotData$type, levels = c("Price in USD","Price Change 2", "Price SD 5"))
  ggplot(plotData) +
    annotation_custom(grid::textGrob(coin, gp=grid::gpar(fontsize=96, alpha=0.05)),xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    geom_line(aes(x=time, y=value)) +
    facet_grid(type ~ ., scales="free") +
    #scale_y_continuous(labels = comma) +
    theme_Publication() +
    geom_vline(xintercept = as.POSIXct(timestamp, origin="1970-01-01"), linetype="dashed", color="red") +
    labs(x=paste("Time on",as.Date(as.POSIXct(timestamp, origin="1970-01-01"))), y="") +
    scale_x_continuous(trans = scales::time_trans(tz = "UTC"))
}

getWindowStats <- function(coin, timestamp, windowSize = 60) {
  print(paste(coin, timestamp))
  pumpdata <- getPump(coin, timestamp, windowSize)
  if(nrow(pumpdata) == 0) {
    return(data.table())
  }
  totalVolume <- max(pumpdata$qusd) - min(pumpdata$qusd)
  peakGainInsiders <- max(pumpdata$cusd) / pumpdata$cusd[1]
  peakGainPublic <- max(pumpdata[loctime > timestamp]$cusd) / pumpdata[order(abs(loctime-timestamp))]$cusd[1]
  secondsToPeak <- pumpdata[cusd == max(pumpdata$cusd)]$loctime[1] - pumpdata[order(abs(loctime-timestamp))]$loctime[1]
  
  # estimate maximum profits
  pumpdata[, volsec := c(0, diff(qusd-min(qusd)))]
  pumpdata[, volsec := ifelse(volsec < 0, 0, volsec)]
  pumpdata[, maxprofit := volsec * (cusd / pumpdata$cusd[1] - 1)]
  maxProfit <- sum(pumpdata[cusd >= pumpdata$cusd[1]]$maxprofit)
  
  initialPrice <- pumpdata$cusd[1]
  initialDayVol <- pumpdata$qusd[1]
  
  meanPriceAfterPump <- mean(pumpdata[loctime > timestamp]$cusd)
  mediumTermPriceIncrease <- meanPriceAfterPump/initialPrice
  
  return(list(symbol=coin,
              timestamp=timestamp,
              totalVolume=totalVolume,
              peakGainInsiders=peakGainInsiders,
              peakGainPublic=peakGainPublic,
              secondsToPeak=secondsToPeak,
              maxProfit=maxProfit,
              initialPrice=initialPrice,
              initialDayVol=initialDayVol,
              mediumTermPriceIncrease=mediumTermPriceIncrease))
}

getMedianVolume <- function(coin) {
  if(!exists(paste0(coin,"BTC"))) {
    eval(parse(text=paste0(coin,"BTC <<- readPair('",coin,"BTC')")))
  }
  return(median(eval(parse(text=paste0(coin,"BTC$qusd")))))
}

getAllStats <- function(pumps, windowSize = 60) {
  res <- do.call(rbind.data.frame,
                 apply(pumps[, c('symbol', 'timestamp')], 1,
                       function(y) getWindowStats(y['symbol'],as.numeric(y['timestamp']), windowSize = windowSize)))
  stats <- pumps[res, on=list(symbol, timestamp)]
  return(stats)
}

tryGetAllStats <- function(pumps, windowSize = 60) {
  res <- do.call(rbind.data.frame,
                 apply(pumps[, c('symbol', 'timestamp')], 1,
                       function(y) {tryCatch({getWindowStats(y['symbol'],as.numeric(y['timestamp']), windowSize = windowSize)},
                                            error={return(data.table())},
                                            warning={return(data.table())}
                                            )}
                       ))
  stats <- pumps[res, on=list(symbol, timestamp)]
  return(stats)
}

getAllBTCVols <- function(pumps, windowSize = 60) {
  all <- do.call(rbind.data.frame,
                 apply(pumps, 1,
                       function(y) {
                         id <- as.numeric(y['id'])
                         print(paste(id, y['symbol'], y['timestamp']))
                         res <- getPumpBTCVols(y['symbol'],as.numeric(y['timestamp']), windowSize = windowSize)
                         if(nrow(res) == 0) {
                           return(data.table())
                         }
                         res$symbol <- y['symbol']
                         res$type <- y['type']
                         res$id <- id
                         return(res)
                       } ))
  return(all)
}

pumpstats <- getAllStats(pumps[order(time)], windowSize = 0.5*60*60)
fwrite(pumpstats, "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/pumpstats.csv", row.names = F)
goodPumps <- pumpstats[totalVolume >= 50000 & peakGainInsiders >= 1.05][, list(symbol, timestamp, type="pump")]
fwrite(goodPumps, "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/goodPumps.csv", row.names = F)
tweetTimes <- fread("~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/tweets.csv")
nonPumps <- tweetTimes[timestamp >= as.numeric(as.POSIXct("2018-07-05"))][order(retweet_count, decreasing = T)][, head(.SD, 20), by=symbol]
nonPumps <- nonPumps[, list(symbol, timestamp, type="non-pump")][order(timestamp, decreasing = F)]
nonpumpstats <- getAllStats(nonPumps[order(timestamp)], windowSize = 4*60*60)
fwrite(nonpumpstats, "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/nonpumpstats.csv", row.names = F)

allTimestamps <- rbind(goodPumps, nonPumps)[order(timestamp, decreasing=FALSE)]
allTimestamps <- allTimestamps[timestamp >= as.numeric(as.POSIXct("2018-07-05"))]
allTimestamps[, id := .I]
allData <- getAllBTCVols(allTimestamps, 0.5*60*60)
fwrite(allData, "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/allData30mintop20.csv")
allData <- getAllBTCVols(allTimestamps, 1*60*60)
fwrite(allData, "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/allData1hrtop20.csv")
allData <- getAllBTCVols(rbind(allTimestamps[, list(symbol, timestamp = timestamp-20*60, type, id=10000+id)],
                               allTimestamps[, list(symbol, timestamp = timestamp-10*60, type, id=20000+id)],
                               allTimestamps,
                               allTimestamps[, list(symbol, timestamp = timestamp+10*60, type, id=30000+id)],
                               allTimestamps[, list(symbol, timestamp = timestamp+20*60, type, id=40000+id)])[order(timestamp)])
                               
fwrite(allData, "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/allData1hrdev10dev20top20.csv")

# relative windowed rolling sum || rolling time interval
#(counts = setDT(df)[.(tmin=Timestamp, tmax=Timestamp+10*60), on=.(Timestamp>tmin, Timestamp<tmax), .(counts=sum(ticket_count)), by=.EACHI]$counts)

POLYPumpPlot <- plotPump("POLY", 1539802863)
ggsave(filename = "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/images/POLYpump.pdf", plot = POLYPumpPlot, width = 6, height = 5)

RLCnewsPlot <- plotPump("RLC", 1552406932, 4*60*60)
ggsave(filename = "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/images/RLCnewsplot.pdf", plot = RLCnewsPlot, width = 4.5, height = 5)

# plot all pump images
for (i in 1:nrow(pumps)) {
  pumpSymbol <- pumps[i]$symbol
  pumpTime <- pumps[i]$timestamp
  print(paste("Working on",pumpSymbol,pumpTime))
  plot <- plotPump(pumpSymbol, pumpTime, windowSize = 2*10*60)
  ggsave(filename = paste0("/tmp/pumps/",pumpSymbol,"-",pumpTime,".png"), plot = plot, width = 6, height = 5)
}


# TODO changerate plot
plotFeaturesReconstruction <- function(coin, timestamp, window = 60) {
  POLYpump <- getPump(coin, timestamp, windowSize = window)
  POLYpump[, time := as.POSIXct(loctime, origin="1970-01-01")]
  POLYchangeRate <- POLYpump[, list(time, value = cusd/shift(cusd)-1, type="Change Rate")]
  POLYpump <- POLYpump[, list(time, value=cusd, type="Price in USD")]
  POLYrse <- fread(paste0("~/workspaces/data/cryptocoins/binance/finaleval/06_12_18/error_",coin,"BTC"))
  #POLYrse <- fread("/tmp/error_POLYBTC")
  POLYrse <- POLYrse[unix < timestamp+window & unix > timestamp-window]
  POLYrse <- POLYrse[, list(time = as.POSIXct(unix, origin="1970-01-01"), value=reconstruction_error, type="Rec. Error")]
  POLYfeatures <- rbind(POLYpump, POLYchangeRate, POLYrse)
  POLYfeatures$type <- factor(POLYfeatures$type, levels = c("Price in USD","Change Rate", "Rec. Error"))
  
  featurePlot <- ggplot(POLYfeatures) +
    annotation_custom(grid::textGrob(coin, gp=grid::gpar(fontsize=96, alpha=0.05)),xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    geom_line(aes(x=time, y=value)) +
    facet_grid(type ~ ., scales = "free") +
    theme_Publication() +
    labs(x=paste("Time on",as.Date(as.POSIXct(timestamp, origin="1970-01-01"))), y="")# +
    #scale_x_continuous(trans = scales::time_trans(tz = "UTC"))
  
  return(featurePlot)
}
featurePlot <- plotFeaturesReconstruction("POLY", 1539802863, 300)
ggsave(filename = "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/images/featureplot.pdf", plot = featurePlot, width = 6, height = 8)


# pump categories plot
TRIGplot <- plotPump("TRIG", 1535990406, 3600*6, withVolume = FALSE)
EVXplot <- plotPump("EVX", 1533308404, 3600*6, withVolume = FALSE)
VIBplot <- plotPump("VIB", 1534420802, 3600*6, withVolume = FALSE)
categoryPlot <- cowplot::plot_grid(TRIGplot, VIBplot, EVXplot, labels = c('1) Sustained pump', '2) Short term pump and dump', '3) Failed pump'), nrow=3)
cowplot::ggsave(filename = "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/images/pumpcategories.pdf", plot = categoryPlot, width = 6, height = 7)


# market caps
CMCsupply <- as.data.table(CMCcoins[, c("symbol","circulating_supply")])#[, list(symbol, circulating_supply)]
CMCsupply[symbol == "YOYOW", symbol := "YOYO"]
CMCsupply <- rbind(CMCsupply, data.table(symbol="TRIG", circulating_supply=32105578))
CMCsupply <- CMCsupply[, list(circulating_supply=max(circulating_supply)), by=symbol]

candidates <- fread("~/workspaces/data/cryptocoins/binance/candidatesEvaluated.csv")
candidates[, time := as.POSIXct(unix, origin="1970-01-01")]
candidates[, minuteDeviation := abs(abs(as.POSIXlt(round(time, "mins"))$min %% 30 - 15)-15)]
additional <- candidates[detected == 1 & known_pump == 0 & minuteDeviation <= 1 & unix <= 1540666189, list(symbol, timestamp = unix)]

allPumps <- rbind(pumps[, list(symbol, timestamp, Type="with evidence")],
                  additional[, list(symbol, timestamp, Type="found additionally")])

stats <- getAllStats(allPumps, 3600)
stats <- merge(stats, CMCsupply)
stats[, marketCap := initialPrice * circulating_supply]
print(paste("There are", nrow(stats[initialPrice == 0]), "pumps that are really just new price listings. We omit them."))
stats <- stats[initialPrice > 0]
marketCapsPlot <- ggplot(stats) +
  geom_histogram(aes(x=marketCap/10^6, fill=Type), color="white", binwidth = 10) +
  theme_Publication() +
  labs(x = "Cryptocurrency market capitalization in million $", y="Number of pumps")
ggsave(filename = "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/images/marketcaps.pdf", plot = marketCapsPlot, width = 6, height = 3)

# trading volume prior to pump
stats$medianVolume <- sapply(stats$symbol, getMedianVolume)
stats$Type <- factor(stats$Type, levels = c("with evidence","found additionally")) # reorder
tradingVolPlot <- ggplot(stats) +
  geom_boxplot(aes(y=initialDayVol/10^6, x=Type)) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  theme_Publication() +
  labs(x="", y="24 hour trading volume, prior to pump")
ggsave(filename = "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/images/tradingVolume.pdf", plot = tradingVolPlot, width = 6, height = 4)

# trading volume in pump and maximum profit
stats <- stats <- getAllStats(allPumps, 300)[initialPrice != 0]
volumeAndProfit <- rbind(stats[, list(Type="Maximum Operator Profit", value=maxProfit)],
                         stats[, list(Type="Total trading volume", value=totalVolume)])
volumeAndProfit$Type <- factor(volumeAndProfit$Type, levels = c("Total trading volume","Maximum Operator Profit"))
volumeAndProfitPlot <- ggplot(volumeAndProfit) + geom_boxplot(aes(x=Type, group=Type, y=value)) +
  scale_y_log10(labels = scales::comma, breaks=10^(1:7), limits=c(10, 10^7)) +
  annotation_logticks(sides="l") +
  theme_Publication() +
  labs(x="", y="Value in USD")
ggsave(filename = "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/images/volumeandprofit.pdf", plot = volumeAndProfitPlot, width = 6, height = 3)


allSymbols <- gsub('.{7}$', '', list.files("~/workspaces/data/cryptocoins/binance/pairs/", pattern = "BTC.csv"))

getDailyPrices <- function(coin) {
  pair <- paste0(coin,"BTC")
  print(pair)
  if(!exists(pair)) {
    eval(parse(text=paste0(coin,"BTC <- readPair('",coin,"BTC')")))
  }
  result <- eval(parse(text=paste0(coin,"BTC[, list(price = median(cusd), symbol=coin), by=list(date=cut(time, '1 day'))]")))
  return(result)
}
#allDailyPrices <- do.call(rbind.data.frame,lapply(allSymbols, getDailyPrices))
allDailyPrices <- fread("~/workspaces/data/cryptocoins/binance/allDailyPrices.csv")[order(symbol, date)]
allDailyPrices$symbol <- gsub('.{3}$', '', allDailyPrices$symbol)
allDailyPrices[, change := price/shift(price), by=symbol]

getPumpPerformance <- function(pumpSymbol, timestamp, instance = 1) {
  pumpDate <- as.Date(as.POSIXct(timestamp, origin="1970-01-01"))
  X <- allDailyPrices[date >= pumpDate]
  X[, day := 1:.N, by=symbol]
  X <- X[, list(day, cumChange = cumprod(change)-1), by=symbol]
  PumpedCoin <- X[symbol == pumpSymbol, list(cumChange, day)]
  AllOthers <- X[symbol != pumpSymbol, list(medianCumChange = median(cumChange, na.rm = T)), by=day]
  return(merge(AllOthers, PumpedCoin)[, list(day, cumChange, medianCumChange, performance=cumChange-medianCumChange, instance=instance)])
}

totalPerformance <- do.call(rbind.data.frame,
        apply(allPumps[, list(symbol, timestamp, instance = .I)], 1,
              function(y) getPumpPerformance(y['symbol'], as.numeric(y['timestamp']), y['instance'])))

performancePlot <- ggplot(totalPerformance[day <= 100]) +
  geom_boxplot(aes(y=performance, group=day, x=day)) +
  scale_y_continuous(labels=scales::percent, breaks=c(-0.5,-0.25,0,0.25,0.5,0.75,1)) +
  coord_cartesian(ylim=c(-0.6, 1)) +
  theme_Publication() +
  labs(x="Days since pump", y="Absolute performance differences to all other coins")
ggsave(filename = "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/images/performances.pdf", plot = performancePlot, width = 6, height = 5)


do.call(rbind.data.frame,
        apply(pumps[, c('symbol', 'timestamp')], 1,
              function(y) getWindowStats(y['symbol'],as.numeric(y['timestamp']), windowSize = windowSize)))



# cv evaluation
cv_results <- fread("~/workspaces/data/cryptocoins/binance/cv_results.csv")
CVplot <- ggplot(cv_results[, list(fold, recall, precision, F1,
                         meanRecall=cumsum(recall)/.I, meanPrecision=cumsum(precision)/.I,
                         meanF1 = cumsum(F1)/.I)],
       aes(x=fold)) +
  #geom_point(aes(y=recall, color="recall")) +
  geom_line(aes(y=meanRecall, linetype="Recall")) +
  #geom_point(aes(y=precision, color="precision")) +
  geom_line(aes(y=meanPrecision, linetype="Precision")) +
  #geom_point(aes(y=F1, color="F1")) +
  geom_line(aes(y=meanF1, linetype="F1")) +
  labs(x="Run in 5-fold Cross Validation", y="Value", linetype="Mean:") +
  theme_Publication()
ggsave(filename = "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/images/CVplot.pdf", plot = CVplot, width = 6, height = 3)

