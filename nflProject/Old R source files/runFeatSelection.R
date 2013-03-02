dateString = 'Jan15'
source('makeModelMatrix.R')
source('featSelFuncs.R')
source('subsetFuncs.R')
require(gbm)

validateWeeks = 5:16
validateSeas = 2001:2009

numWeeks = 32
numSeas = 4

folds = MMFoldIndexList(dat, numWeeksFixed = F, validateWeeks, validateSeas, numSeas = numSeas)

features = genFeatSubset(MM, validateWeeks, makeNewSubset = T, prevSubset = NULL,
                         excludeByName = c('Season', 'Week', 'Home', 'Away', 'HomeMargin'),
                         subsetSizeFunc = subsetSizeFunc, perturbFunc = perturbFunc, 
                         excludeFeatFunc = excludeFeatFunc, minNfeats= 500, maxNfeats = 800)

wanra = gbm(HomeMargin ~ ., data = MM[folds[[1]][[100]], c('HomeMargin', features)], distribution = 'gaussian',
            n.trees = 5000)

a = predict.gbm(wanra, newdata = MM[folds[[2]][[100]], c('HomeMargin', features)], n.trees = 2000)
data.frame(s = MM$Season[folds[[2]][[100]]], w = MM$Week[folds[[2]][[100]]], h = MM$Home[folds[[2]][[100]]],
           line = MM$HomePtSpread[folds[[2]][[100]]], myMargin = -a,
           actual = -MM$HomeMargin[folds[[2]][[100]]], myErr = -MM$HomeMargin[folds[[2]][[100]]] + a)