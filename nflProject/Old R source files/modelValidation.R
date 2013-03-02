dateString = 'Feb7'
dataPath = '~/projects/nfl/nflProject/data_objects/'
funcsPath = '~/projects/nfl/nflProject/funcs/'
load(paste(dataPath, 'MM_', dateString, '.Rout', sep = ''))
source(paste(funcsPath, 'genFeatSubsetFuncs.R', sep = ''))
source(paste(funcsPath, 'modelValidationFuncs.v2.R', sep = ''))
require(gbm)
rm(dateString, dataPath, funcsPath)

# external parameters
adjForBye = T
validateWeeks = 14
validateSeas = 2005:2009
numWkFixed = F
if (numWkFixed){numWeeks = 32} else {numSeas = 4}
nLaterGames = 5 # for ex: building rule for wk 7 games and nLaterGames = 5, training data will include games in weeks 7-12 with features with max k feature = 5; nLater = all uses all games; when nLaterGames + week of validation > 16 then all later games are used
pcaPctMaxScoreToExlcude = .05
plotPCAscores = T
# internal gbm parameters
numTrees = 50
minObs = 5
intDepth = 1

kValue = gsub('[[:alpha:]]+([[:digit:]][[:digit:]]*).+', '\\1', colnames(MM))
kMax = max(na.exclude(as.numeric(kValue)))
colToExclude = ifelse(is.na(as.numeric(kValue)), FALSE, as.numeric(kValue) + as.numeric(adjForBye) + 1 > min(validateWeeks))
data = MM[,!colToExclude]
data = imputeMedAndReturn(data)
data = data[, -(1:4)]
data = data[, -which(colnames(data)=='HomeMargin')]

pca = princomp(~., data)
minIncl = which.min(pca$sdev >  max(pca$sdev)*pcaPctMaxScoreToExlcude)
if (plotPCAscores){
  plot(pca$sdev)
  abline(h = pca$sdev[minIncl])
  abline(v = minIncl)
}
pcaMM = as.matrix(data) %*% pca$loading[, 1:minIncl]
pcaMM = cbind(MM[,1:4], HomeMargin = MM$HomeMargin, pcaMM)

folds = MMFoldIndexList(pcaMM, numWeeksFixed = numWkFixed, validateWeeks, validateSeas, numSeas = numSeas,
                        numLaterGames = 17, trainWithWk17 = F, inclEarlierGames = T, maxK = kMax)
features = colnames(pcaMM)[-(1:5)]

gbmStats = fitGbmReturnMeanWeekSummStats(pcaMM, features, MM$HomePtSpread, folds, betFunc, 0, 
                                         n.trees = numTrees, n.minobsinnode = minObs, interaction.depth = intDepth)

