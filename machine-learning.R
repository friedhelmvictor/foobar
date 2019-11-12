library(caret)
#library(MLmetrics)
library(data.table)
library(tsfeatures)
library(doMC)
registerDoMC(cores = 4)

# Import dataset
data <- fread("~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/allData4hrtop5.csv")
data <- fread("~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/allData2hrtop20.csv")
data <- fread("~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/allData1hrtop20.csv") ####
data <- fread("~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/allData1hrdev10dev20top20.csv")
data <- fread("~/Documents/Eigene Paper/work in progress/wip2018pumpanddump/code/allData30mintop20.csv")
data[type == "non-pump", type := "regular"]
data$type <- as.factor(data$type)
data <- data[id %in% data[, list(count = .N), by=id][count >=5]$id] # minimum of 5 observations per group
data <- data[!(id %in% data[, list(uniquechanges = length(unique(c/shift(c)))), by=id][uniquechanges<3]$id)]

# Compute features
# Set up progress bbar by number of groups
numberOfGroups = uniqueN(data$id)
progressBar <- txtProgressBar(min = 0, max = numberOfGroups, style = 3)

featuredata <- data[, {
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
by=list(id, type, symbol)]

# choose top 5:
#featuredata <- featuredata[, list(return5minToPeak, loss5minAfterPeak, peakmin1, tdiffmotiftwo_entro3, tdiffentropy, type)] # 2hr top20
#featuredata <- featuredata[, list(peakmin1, cchangestd1st_der, cchangeoutlierinclude_mdrmd, return5minToPeak, peakToMedian, type)] #1hr top20 nnet
#featuredata <- featuredata[, list(peakmin1, return5minToPeak, tdiffflat_spot, loss5minAfterPeak, tdifflumpiness, type)] #30min top20 nnet

prevcount <- nrow(featuredata)
featuredata <- na.omit(featuredata)
print(paste("Dropped", prevcount-nrow(featuredata), "samples."))

set.seed(99)
train.index <- createDataPartition(featuredata$type, p = .5, list = FALSE)
train <- featuredata[ train.index,]
test  <- featuredata[-train.index,]

# Store X and Y for later use.
x = as.data.table(as.data.frame(train)[, setdiff(colnames(train), c("id", "type", "symbol"))])
y = train$type

#preProcValues <- preProcess(x, method = c("center", "scale"))
#x <- predict(preProcValues, x)
#test <- predict(preProcValues, test)

library(ModelMetrics)
customSummary <- function (data, lev = NULL, model = NULL) 
{
  # print(data)
  # lvls <- levels(data$obs)
  # out <- c(1,1,1)
  # names(out) <- c("ROC", "Sens", "Spec")
  # out
  
  pred <- prediction(data$pump , data$obs)
  perf <- performance(pred,"tpr","fpr")
  cutoffs <- data.table(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                        tpr=perf@y.values[[1]])
  #print(cutoffs)
  return(unlist(cutoffs[fpr == 0][order(tpr, decreasing = T)][, list(threshold = cut, fpr, tpr)][1]))
}

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10,
  ## Estimate class probabilities
  classProbs = TRUE,
  allowParallel = TRUE,
  savePredictions = TRUE,
  search = "random", # random parameter search
  summaryFunction = twoClassSummary) #prSummary | twoClassSummary

gbmFit1 <- train(x=x, y=y, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = TRUE,
                 tuneLength = 10,
                 ## Specify which metric to optimize
                 metric = "ROC",
                 preProcess = c("center", "scale"))
gbmFit1
summary(gbmFit1)
saveRDS(gbmFit1, "gbm30minAllF.rds")

nnetFit1 <- train(x=x, y=y, 
                  method = "nnet", 
                  trControl = fitControl,
                  ## This last option is actually one
                  ## for gbm() that passes through
                  verbose = TRUE,
                  tuneLength = 10,
                  ## Specify which metric to optimize
                  metric = "ROC",
                  preProcess = c("center", "scale"))
nnetFit1
saveRDS(nnetFit1, "nnet30minAllF.rds")

library(xgboost)

xgb_grid_1 = expand.grid(
  nrounds = 1000,
  max_depth = c(2, 4, 6, 8, 10, 12, 14),
  eta=c(0.01, 0.005, 0.001, 0.0005),
  gamma = 0.01,
  colsample_bytree=0.5,
  min_child_weight=c(0.25, 0.5, 1),
  subsample=0.5
)

xgb_train_1 = train(
  x = as.matrix(x),
  y = as.matrix(y),
  trControl = fitControl,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)
saveRDS(xgb_train_1, "xgb30minAllF.rds")

## ROC stuff
library(pROC)
selectedIndices <- nnetFit1$pred$size == 11
plot.roc(nnetFit1$pred$obs[selectedIndices],
         nnetFit1$pred$regular[selectedIndices])


library(ROCR)
pred <- prediction( predict(xgb_train_1, newdata = test, type = "prob")$regular, test$type)
perf <- performance(pred,"tpr","fpr")
plot(perf)
cutoffs <- data.table(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs[fpr == 0][order(tpr, decreasing = T)]

## !ROC stuff

table(actual=test$type, predicted=predict(xgb_train_1, newdata = test))
foor <- as.data.table(predict(gbmFit1, newdata = test, type = "prob"))
foor$actual <- test$type
foor$id <- test$id

######### test alert
subsets <- c(1:10)
caretFuncs$summary <- twoClassSummary
ctrl <- rfeControl(functions = caretFuncs,
                   method = "cv",
                   repeats = 10, number = 10,
                   verbose = T)

nnetProfile <- rfe(x, y,
                   sizes = subsets,
                   rfeControl = ctrl,
                   method = "nnet",
                   metric = "ROC",
                   trControl = fitControl)

gbmProfile <- rfe(x, y,
                  sizes = subsets,
                  rfeControl = ctrl,
                  method = "gbm",
                  metric = "ROC",
                  trControl = fitControl)

nnetProfile
######### test alert

ggplot(data[id==8]) + geom_line(aes(x=as.POSIXct(loctime, origin="1970-01-01"), y=c))






## apply to new data
somecoin <- fread("~/workspaces/data/cryptocoins/binance/pairs/CHATBTC.csv")[order(loctime, decreasing = F)]
somecoin <- extractVolBars(somecoin)
somecoin[, time := loctime]
# relative windowed rolling sum || rolling time interval
(counts = somecoin[.(loctime, tmin=loctime, tmax=loctime+2*60*60), on=.(loctime>tmin, loctime<tmax), .(counts=nrow(.SD)), by=.EACHI]$counts)
# somecoin$counts <- counts
# somecoin <- somecoin[counts >= 5]
# somecoin <- somecoin[1:nrow(somecoin)-1]
getFeature <- function(somecode) {
  return(somecoin[.(tmin=loctime, tmax=loctime+2*60*60),
                  on=.(loctime>=tmin, loctime<tmax),
                  .(value = as.numeric(ifelse(nrow(.SD) > 2, eval(parse(text=somecode)), NA))),
                  by=.EACHI]$value)
}

somecoin$return5minToPeak <- getFeature("max(.SD$c) / .SD[, list(c, t = abs(loctime - (timestampMaxC-5*60)))][order(t, decreasing = F)][1]$c")
somecoin$loss5minAfterPeak <- getFeature("max(.SD$c) / .SD[, list(c, t = abs(loctime - (timestampMaxC+5*60)))][order(t, decreasing = F)][1]$c")
somecoin$peakmin1 <- getFeature("abs((as.POSIXlt(.SD[c/shift(c) == max(.SD[, list(return=c/shift(c))], na.rm = T)]$time, origin='1970-01-01')$min+15)%%30-15)")
somecoin$tdiffmotiftwo_entro3 <- getFeature("motiftwo_entro3(na.omit(time-shift(time)))")
somecoin$tdiffentropy <- getFeature("entropy(time-shift(time))")
somecoin$tdiffflat_spot <- getFeature("flat_spots(na.omit(loctime-shift(loctime)))")
somecoin$tdifflumpiness <- getFeature("lumpiness(na.omit(loctime-shift(loctime)))")

somecoin[, time := as.POSIXct(loctime, origin="1970-01-01")]
somecoin$predicted <- predict(gbmFit1, newdata = somecoin)
ggplot(somecoin) + geom_line(aes(x=time, y=c, color=predicted, group=NA))
