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

initFeatures = genFeatSubset(MM, validateWeeks, makeNewSubset = T, prevSubset = NULL,
                         excludeByName = c('Season', 'Week', 'Home', 'Away', 'HomeMargin'),
                         subsetSizeFunc = subsetSizeFunc, perturbFunc = perturbFunc, 
                         excludeFeatFunc = excludeFeatFunc, minNfeats= 50, maxNfeats = 100)
initFeatures = c(initFeatures, 'HomePtSpread')

# numTrees = validateTuningParameter()
numTrees = 2000
# features = featSelect()
features = initFeatures

gbmPerformance = fitModelReturnMeanWeekSummStats(MM, features, folds, betFunc, 1, n.trees = numTrees)
apply(gbmPerformance, 2, mean)

fitModelReturnMeanWeekSummStats = function(data, features, folds, betFunc, delta, ...){
  MM = data[,c(features, 'HomeMargin')]
  counter = 1
  rmses = c()
  winpcts = c()
  for (weekIndex in 1:36){#length(folds[[1]])){
    fit = gbm(HomeMargin ~ ., data = MM[folds[[1]][[weekIndex]],],
              distribution = 'gaussian', ...)
    myLine = -predict.gbm(fit, newdata = MM[folds[[2]][[weekIndex]], c('HomeMargin', features)], 
                    n.trees = numTrees)
    ptSpread = MM[folds[[2]][[weekIndex]], 'HomePtSpread']
    actual = MM[folds[[2]][[weekIndex]], 'HomeMargin']
#     betOn = ifelse(myLine - ptSpread < 0, 'H', 'A')
    betOn = betFunc(myLine, ptSpread, delta)
    betWinner = ifelse(actual + ptSpread > 0, 'H', ifelse(actual + ptSpread == 0, 'T', 'A'))
    tieIndex = betWinner == 'T'
    weekWinPct = mean(na.exclude(betOn[!tieIndex] == betWinner[!tieIndex]))
    rmse = sqrt(mean((myLine - actual)^2))
    pctGameBet = length(na.exclude(betOn[!tieIndex]))/sum(!tieIndex)
    rmses[counter] = rmse
    winpcts[counter] = weekWinPct
    pctGamesBet[counter] = pctGameBet
    counter = counter + 1
  }
  return(data.frame(rmse = rmses, winPct = winpcts, pctBet = pctGamesBet))
}

betFunc = function(myLine, ptSpread, delta){
  homeAwayOrNA = ifelse(myLine - ptSpread < -delta, 'H', 
                        ifelse(myLine - ptSpread > delta, 'A', NA))
  return(homeAwayOrNA)
}


plot(5:16, rmses)
plot(5:16, winpcts)
mean(winpcts)
trees = seq(1000, 5000, by = 250)
counter = 1
rmses = c()
winpcts = c()
for (numTree in trees){
  a = predict.gbm(fit, newdata = MM[folds[[2]][[weekIndex]], c('HomeMargin', features)], 
                  n.trees = numTree)
  myLine = -a
  ptSpread = MM[folds[[2]][[weekIndex]], 'HomePtSpread']
  actual = MM[folds[[2]][[weekIndex]], 'HomeMargin']
  betOn = ifelse(myLine - ptSpread < 0, 'H', 'A')
  betWinner = ifelse(actual + ptSpread > 0, 'H', ifelse(actual + ptSpread == 0, 'T', 'A'))
  weekWinPct = mean(betOn == betWinner)
  rmse = sqrt(mean((myLine - ptSpread)^2))
  weekWinPct; rmse; data.frame(round(myLine,2), ptSpread, -actual, betOn, betWinner)
  rmses[counter] = rmse
  winpcts[counter] = weekWinPct
  counter = counter + 1
  print(numTree)
}
plot(trees, rmses)
plot(trees, winpcts)

summary.gbm(fit)$var[summary.gbm(fit)$rel.inf != 0]
