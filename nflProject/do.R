fitWindowSize = 45

cleanDateString = 'Mar18'
MMpath = '~/GitHub/NFL/nflProject/data_objects/'
load(paste(MMpath, 'MM_', cleanDateString, '.Rout', sep = ''))
source('~/GitHub/NFL/nflProject/funcs.R')
rm(cleanDateString, MMpath)

# external parameters
validateWeeks = 2:16
validateSeas = 1990:2009
numWeeks = fitWindowSize

folds = MMFoldIndexList(MM, validateWeeks, validateSeas, numWeeks = numWeeks)

# to add new features using data held in allDat, use ... in dataToMM to pass args to 
# featureOrganize func to exclude the first N columns of the df; cbind to attach to
# the loaded MM (Jan 15 MM took ~ 45 sec per season or about 21 minutes)

require(gbm)
pcaPctMaxScoreToExlcude = .05
plotPCAscores = T
modelString = 'gbm'
# internal gbm parameters
numTrees = 50
minObs = 5
intDepth = 1



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
