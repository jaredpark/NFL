dateString = 'Feb7'
dataPath = '~/projects/nfl/nflProject/data_objects/'
funcsPath = '~/projects/nfl/nflProject/funcs/'
load(paste(dataPath, 'MM_', dateString, '.Rout', sep = ''))
source(paste(funcsPath, 'genFeatSubsetFuncs.R', sep = ''))
source(paste(funcsPath, 'modelValidationFuncs.R', sep = ''))
require(gbm)
rm(dateString, dataPath, funcsPath)

kValue = gsub('[[:alpha:]]+([[:digit:]][[:digit:]]*).+', '\\1', colnames(MM))
kMax = max(na.exclude(as.numeric(kValue)))
adjForBye = T

# results = NULL
# vGroups = 
# for (validateWeeks in vGroups){
validateWeeks = 14
validateSeas = 2005:2009
numWeeks = 32
numSeas = 4
nLaterGames = 5 # for ex: building rule for wk 7 games and nLaterGames = 5, training data
# will include games in weeks 7-12 with features with max k feature = 5; nLater = all uses all games;
# when nLaterGames + week of validation > 16 then all later games are used


excludeCol = ifelse(is.na(as.numeric(kValue)), FALSE, as.numeric(kValue) + as.numeric(adjForBye) + 1 > min(validateWeeks))
dat = MM[,!excludeCol]

data = imputeMedAndReturn(dat)
data = data[, -(1:4)]
data = data[, -which(colnames(data)=='HomeMargin')]
pca = princomp(~., data)
plot(pca$sdev)
abline(h = max(pca$sdev)*.05)
minIncl = which.min(pca$sdev >  max(pca$sdev)*.05)
abline(v = minIncl)

pcaMM = as.matrix(data) %*% pca$loading[, 1:minIncl]
pcaMM = cbind(MM[,1:4], HomeMargin = MM$HomeMargin, pcaMM)

source(paste(funcsPath, 'modelValidationFuncsTesting.R', sep = ''))
folds = MMFoldIndexList(pcaMM, numWeeksFixed = F, validateWeeks, validateSeas, numSeas = numSeas,
                        numLaterGames = 17, trainWithWk17 = F, inclEarlierGames = T, maxK = kMax)
sum(folds[[1]][[3]])

numTrees = 50

features = colnames(pcaMM)[-(1:5)]
gbmStats = fitGbmReturnMeanWeekSummStats(pcaMM, features, MM$HomePtSpread, folds, betFunc, 0, 
                                         n.trees = numTrees, n.minobsinnode = 5, interaction.depth = 1)
# apply(gbmStats, 2, mean)
results = rbind(results, apply(gbmStats, 2, mean))
}
apply(results, 2, mean)