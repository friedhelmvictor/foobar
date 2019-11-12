library(caret)
library(data.table)
library(tsfeatures)
setwd("~/workspaces/R/pumppred/")

###################### load model ########################


extractVolBars <- function(coin) {
  coin[, volume := ifelse(n > shift(n), pmax(0, q-shift(q)),
                          ifelse(c != shift(c) & n < shift(n), pmax(0, shift(q) - ((shift(n) - n) * (q/n)) - q), 0))]
  
  coin <- coin[, list(loctime = loctime/1000, c, q, n, time = as.POSIXct(loctime/1000, origin="1970-01-01"), volume)]
  coinTicks <- coin[volume != 0]
  coinVols <- coinTicks[, list(loctime, c, cumvol = round(cumsum(volume)/0.1))][cumsum(rle(cumvol)$lengths)]
}

my_model <- readRDS("xgb30minAllF.rds")
##########################################################


###################### load data #########################
sourceDir <- "~/workspaces/data/cryptocoins/binance/pairs/fixed/"
allFiles <- list.files(sourceDir)


findPumps <- function(filename) {
  coin <- gsub('.{7}$', '', filename)
  somecoin <- fread(paste0(sourceDir, filename))[order(loctime, decreasing = F)]
  somecoin <- extractVolBars(somecoin)
  
  # relative windowed rolling sum || rolling time interval
  somecoin[, time := loctime]
  getFeature <- function(somecode) {
    return(somecoin[.(tmin=loctime, tmax=loctime+2*60*60),
                    on=.(loctime>=tmin, loctime<tmax),
                    .(value = as.numeric(ifelse(nrow(.SD) > 2, eval(parse(text=somecode)), NA))),
                    by=.EACHI]$value)
  }
  
  
  minT <- as.POSIXct(min(somecoin$loctime), origin="1970-01-01")
  minDate <- as.Date(minT)-1
  customMinT <- as.POSIXct(minDate) + 15*60
  fakeRow <- somecoin[1]
  fakeRow$loctime <- as.numeric(customMinT)
  foo <- rbind(fakeRow, somecoin)
  foo[, time := as.POSIXct(loctime, origin="1970-01-01")]
  foo[, .SD, by=list(time=as.POSIXct(cut(time, "30 mins"))+15*60)]
  foo <- foo[, .SD, by=list(time=as.POSIXct(cut(time, "30 mins"))+15*60)]
  validTimes <- foo[, list(count = .N), by=time][count >= 5]$time
  foo <- foo[time %in% validTimes]
  
  print(paste("building features for", filename))
  numberOfGroups = uniqueN(foo$time)
  progressBar <- txtProgressBar(min = 0, max = numberOfGroups, style = 3)
  
  features <- foo[, {
    setTxtProgressBar(progressBar, .GRP);
    timestampLargestReturn <- head(.SD[c/shift(c) == max(.SD[, list(return=c/shift(c))], na.rm = T)],1)$loctime
    timestampMaxC <- head(.SD[c == max(.SD$c)],1)$loctime
    timestampMinC <- head(.SD[c == min(.SD$c)],1)$loctime
    
    list(valueCount = .N,
         centropy = as.numeric(entropy(c)),
         cstability = stability(c), # not bad
         clumpiness = lumpiness(c), # not bad
         cflat_spot = flat_spots(c),
         ccrossing_points = crossing_points(c),
         churst = hurst(c),
         cunitroot_kpss = unitroot_kpss(c),
         ### cunitroot_pp = unitroot_pp(c), # error!! Error in coef(test.reg)[2, 1] : subscript out of bounds
         ### cnonlinearity = nonlinearity(c), error
         ###cembed2_incircle = embed2_incircle(c), # useless
         cmotiftwo_entro3 = motiftwo_entro3(c),
         ###cwalker_propcross = walker_propcross(c),
         cstd1st_der = std1st_der(c), # not bad
         ###chistogram_mode = histogram_mode(c),
         ################coutlierinclude_mdrmd = outlierinclude_mdrmd(c), # not bad
         
         cchangeentropy = as.numeric(entropy(c/shift(c))),
         cchangestability = stability(na.omit(c/shift(c))),
         cchangelumpiness = lumpiness(na.omit(c/shift(c))),
         ###cchangeflat_spot = flat_spots(na.omit(c/shift(c))),
         cchangecrossing_points = crossing_points(na.omit(c/shift(c))),
         ###cchangehurst = hurst(na.omit(c/shift(c))),
         cchangeunitroot_kpss = unitroot_kpss(na.omit(c/shift(c))),
         ###cchangeunitroot_pp = unitroot_pp(na.omit(c/shift(c))), # error
         ###cchangenonlinearity = nonlinearity(na.omit(c/shift(c))), # error
         ###cchangeembed2_incircle = embed2_incircle(na.omit(c/shift(c))),
         cchangemotiftwo_entro3 = motiftwo_entro3(na.omit(c/shift(c))),
         cchangewalker_propcross = walker_propcross(na.omit(c/shift(c))), # error too short
         cchangestd1st_der = std1st_der(na.omit(c/shift(c))), # too short
         cchangehistogram_mode = histogram_mode(na.omit(c/shift(c))),
         ################cchangeoutlierinclude_mdrmd = outlierinclude_mdrmd(na.omit(c/shift(c))), # really slow, but not bad
         
         
         tdiffentropy = as.numeric(entropy(loctime-shift(loctime))), # best so far
         tdiffstability = stability(na.omit(loctime-shift(loctime))),
         tdifflumpiness = lumpiness(na.omit(loctime-shift(loctime))),
         tdiffflat_spot = flat_spots(na.omit(loctime-shift(loctime))),
         ###tdiffcrossing_points = crossing_points(na.omit(loctime-shift(loctime))),
         tdiffhurst = hurst(na.omit(loctime-shift(loctime))),
         tdiffunitroot_kpss = unitroot_kpss(na.omit(loctime-shift(loctime))),
         tdiffunitroot_pp = unitroot_pp(na.omit(loctime-shift(loctime))),
         ###tdiffnonlinearity = nonlinearity(na.omit(loctime-shift(loctime))),
         ###tdiffembed2_incircle = embed2_incircle(na.omit(loctime-shift(loctime))),
         tdiffmotiftwo_entro3 = motiftwo_entro3(na.omit(loctime-shift(loctime))),
         tdiffwalker_propcross = walker_propcross(na.omit(loctime-shift(loctime))),
         tdiffstd1st_der = std1st_der(na.omit(loctime-shift(loctime))),
         ###tdiffhistogram_mode = histogram_mode(na.omit(loctime-shift(loctime))),
         ################tdiffoutlierinclude_mdrmd = outlierinclude_mdrmd(na.omit(loctime-shift(loctime))),
         
         # # custom ideas
         peakmin1 = abs((as.POSIXlt(timestampLargestReturn, origin="1970-01-01")$min+15)%%30-15),
         tdiffmaxcminc = timestampMaxC - timestampMinC,
         tdiffmaxclargestret = timestampMaxC - timestampLargestReturn,
         return5minToPeak = max(.SD$c) / .SD[, list(c, t = abs(loctime - (timestampMaxC-5*60)))][order(t, decreasing = F)][1]$c,
         loss5minAfterPeak = max(.SD$c) / .SD[, list(c, t = abs(loctime - (timestampMaxC+5*60)))][order(t, decreasing = F)][1]$c,
         peakToMedian = max(.SD$c)/median(.SD$c)
    )},
    by=time]
  
  ############# prediction ###############
  features$pumplikelihood <- predict(my_model, newdata = features, type = "prob")$pump
  fwrite(features[pumplikelihood > 0.999, list(symbol = gsub('.{3}$', '', coin), timestamp = as.numeric(time), time)],paste0(coin,"pumps.csv"))
}
for (file in allFiles) {
  findPumps(file)
}
#ggplot(foo) + geom_line(aes(x=time, y=c)) + geom_vline(data=features[pred == "pump"], aes(xintercept=time), color="red")
