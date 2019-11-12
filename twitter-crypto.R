library(rtweet)
library(data.table)

api_key <- "5Y2iocHOaEU3Kz3OfZDIkpHkX"
api_secret <- "acjDqYp47hLZKGU0PvIBl0lp4VjydCzfdpjTeKH0u5xSoy3uJX"
token <- "1154876193998626821-LvRp7hOIICBUhXQsxrSnBB9MNZvnJw"
token_secret <- "9ad9ta6NLmsUimRp5NXw0WU2Xss9AOb7HgcOnUCFBG6w1"

token <- create_token(
  app = "timeextract",
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = token,
  access_secret = token_secret)

twitteraccounts <- fread("~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/twitteraccounts.csv")[Twitter != ""]

alltweets <- data.table()
for (i in 1:nrow(twitteraccounts)) {
  symbol <- twitteraccounts[i, ]$Symbol
  account <- twitteraccounts[i, ]$Twitter
  if (TRUE) {
    tweets <- as.data.table(get_timeline(account, n=3200))
    withretweets <- rbind(na.omit(tweets[, list(timestamp=as.numeric(retweet_created_at), text=gsub("\r?\n|\r", " ", retweet_text), retweet_count=retweet_retweet_count)]),
                          tweets[, list(timestamp=as.numeric(created_at), text=gsub("\r?\n|\r", " ",text), retweet_count)])
    withretweets$symbol <- symbol
    alltweets <- rbind(alltweets, withretweets)
    alltweets <- alltweets[, list(symbol, timestamp, text, retweet_count)]
  }
  Sys.sleep(10)
}
# group and select tweet with earliest timestamp
alltweets <- alltweets[, list(timestamp = min(timestamp)), by=list(symbol, text, retweet_count)]
fwrite(alltweets, "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/tweets.csv", row.names = F)

selectedTweets <- alltweets[timestamp > as.numeric(as.POSIXct("2018-07-01"))][order(retweet_count, decreasing = T)][, head(.SD, 5), by=symbol]
fwrite(selectedTweets, "~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/selectedtweets.csv", row.names = F)



### irrelevant from here on


tweets <- as.data.table(get_timeline("Cardano", n=3200))
withretweets <- rbind(na.omit(tweets[, list(timestamp=retweet_created_at, text=retweet_text)]), tweets[, list(timestamp=created_at, text)])

LINKBTC <- fread("~/workspaces/data/cryptocoins/binance/LINKBTC.csv")
LINKBTC[, time := as.POSIXct(loctime/1000, origin="1970-01-01")]


ggplot(dplyr::sample_n(LINKBTC, 100000)) + geom_line(aes(x=time, y=c)) + geom_vline(data=timestampsdf, aes(xintercept=timestamp), color="red", alpha=0.5)

for (i in 1:1) {
  windowsize <- 4*60 #minutes
  row <- timestampsdf[timestamp > "2018-07-05"][order(timestamp)][i,]
  time_i <- row$timestamp
  text_i <- row$text
  window <- LINKBTC[time >= time_i - windowsize/2*60 & time <= (time_i + windowsize/2*60)][order(time)]
  firstc <- head(window, 1)$c
  lastc <- tail(window,1)$c
  return <- lastc / firstc
  if(return > 1.04) {
    print(paste("At",time_i,"return is", return, text_i))
  }
  
}

ggplot(dplyr::sample_n(LINKBTC[time >= "2018-07-25" & time <= "2018-07-25 09:00"], 1000)) + geom_line(aes(x=time, y=c)) 

