dateString = 'Jan23'
dataPath = '~/projects/nfl/nflProject/data_objects/'
funcsPath = '~/projects/nfl/nflProject/funcs/'
# makeModelMatrix.R to create and use a new model matrix
load(paste(dataPath, 'gameInfo_', dateString, '.Rout', sep = ''))
load(paste(dataPath, 'allDat_', dateString, '.Rout', sep = ''))
load(paste(dataPath, 'MM_', dateString, '.Rout', sep = ''))
source(paste(funcsPath, 'genFeatSubsetFuncs.R', sep = ''))
source(paste(funcsPath, 'modelValidationFuncs.R', sep = ''))
require(gbm)
rm(dateString, dataPath, funcsPath)

# need to validate over choices of:
# - weeks of season (bin) for each prediction rule (validateWeeks)
# - number of binned prediction rules (NA)
# - number of previous seasons/weeks to use for cross-validation (validateSeas)
# - number of previous seasons/weeks to use for training each week (fold) of the previous seasons used for CV (numWeeks/numSeas)
# - size of feature set used for each fold
# - which features to use in feature set
# - whether to use max(allprevweek, k) for each binned prediction rule vs excluding k > min(weeks in bin) - 2 -- two b/c -1 to avoid bye week complications
# - error metric: just use rmse or validate based on bet performance?
# - if bet performance, choice of bet/no-bet function
# - tuning parameters, tuning parameters, tuning parameters.

validateWeeks = 5:8
validateSeas = 2001:2003

numWeeks = 32
numSeas = 3

folds = MMFoldIndexList(MM, numWeeksFixed = F, validateWeeks, validateSeas, numSeas = numSeas)

initFeatures = genFeatSubset(MM, validateWeeks, makeNewSubset = T, prevSubset = NULL,
                             excludeByName = c('Season', 'Week', 'Home', 'Away', 'HomeMargin'),
                             subsetSizeFunc = subsetSizeFunc, perturbFunc = perturbFunc, 
                             excludeFeatFunc = excludeFeatFunc, minNfeats= 50, maxNfeats = 100)
initFeatures = c(initFeatures, 'HomePtSpread')

# numTrees = validateTuningParameter()
numTrees = 500
# features = featSelect()
features = initFeatures

gbmStats = fitGbmReturnRMSE(MM, features, folds, T, n.trees = numTrees)
mrmse = mean(gbmStats[,1])
mlineRmse = mean(gbmStats[,2])
