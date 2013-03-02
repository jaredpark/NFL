cleanDateString = 'Feb16'
dataPath = '~/projects/nfl/nflProject/data_objects/'
sourceFilePath = './'
load(paste(dataPath, 'cleanData_', cleanDateString, '.Rout', sep = ''))
load(paste(dataPath, 'gameInfo_', cleanDateString, '.Rout', sep = ''))
source(paste(sourceFilePath, 'funcs.v2.R', sep = ''))
rm(cleanDateString, dataPath, sourceFilePath)

# MM settings:
K = 4:6
weeks = 5:8
seas = 2000:2006

# creating model matrix
MM = dataToMM(dat, gameInfo, K, weeks, seas, nothing, featureOrganize, dataToFeatures, 'testing')
MM = dropColWithNA(MM, 'all')
# MM = dataToMM(dat, gameInfo, K, weeks, seas, imputeMedAndReturn, featureOrganize, dataToFeatures, dateString)
# MM = dataToMM(dat, gameInfo, K, weeks, seas, dropColWithNA, featureOrganize, dataToFeatures, dateString)
# rm(dat)
# adding week of game features
ptSpreadAdd = addWeekOfGameFeat(allDat$PointSpread, gameInfo, MM)
MM = cbind(MM, 'HomePtSpread' = ptSpreadAdd)
rm(ptSpreadAdd)
save(MM, file = paste(dataPath, 'MM_', dateString, '.Rout', sep = ''))

# external parameters
adjForBye = T
validateWeeks = 6:8
validateSeas = 2004:2006
numWkFixed = T
if (numWkFixed){numWeeks = 20} else {numSeas = 4}
nLaterGames = 5 # for ex: building rule for wk 7 games and nLaterGames = 5, training data will include games in weeks 7-12 with features with max k feature = 5; nLater = all uses all games; when nLaterGames + week of validation > 16 then all later games are used
pcaPctMaxScoreToExlcude = .05
plotPCAscores = T
modelString = 'gbm'
# internal gbm parameters
numTrees = 50
minObs = 5
intDepth = 1

folds = MMFoldIndexList(MM, numWeeksFixed = numWkFixed, validateWeeks, validateSeas, numSeas = numSeas, numWeeks = numWeeks, F,
                        numLaterGames = 17, trainWithWk17 = F, inclEarlierGamesWithAllInfo = T, maxK = max(K))

# to add new features using data held in allDat, use ... in dataToMM to pass args to 
# featureOrganize func to exclude the first N columns of the df; cbind to attach to
# the loaded MM (Jan 15 MM took ~ 45 sec per season or about 21 minutes)

require(gbm)



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
