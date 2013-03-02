MMFoldIndexList = function(MM, numWeeksFixed = F, weeks, seasons, numSeas, numWeeks, 
                           inclLaterSeasGames, inclEarlierGamesWithAllInfo, maxK = NULL){
  # returns a list with drawer one containing a list of MM indeces for the train data
  # with the second drawer containing a list of MM indeces for the validation data;
  # the i'th element of the first drawer holds an index for MM denoting the train
  # data to be used to evaluate a prediction rule for validation on the MM data indexed
  # by the i'th element of the second drawer.
  data = MM[, c('Week', 'Season')]
  out = list()
  out[[1]] = list()
  out[[2]] = list()
  names(out) = c('train', 'validate')
  # gets rid of weeks not needed and seasons after the last train season
  toValidate = which(is.element(data$Season, seasons) & is.element(data$Week, weeks))
  prevS = 0
  prevW = 0
  counter = 0
  for (row in toValidate){
    s = data[row, 'Season']
    w = data[row, 'Week']
    if (s == prevS & w == prevW){
      # do nothing, already have the train/valid indeces stored in out
    } else {
      counter = counter + 1
      validIndex = data$Week == w & data$Season == s
      if (!numWeeksFixed){
        # if not fixed, this method uses previous x seasons games held in weeks, in addition to all prev
        # in-season games held in week; training a rule for validation on a given week games
        if (inclLaterSeasGames){
          trainIndex = ((data$Season == s) & (data$Week < w & data$Week >= min(weeks))) | (data$Week >= min(weeks) & is.element(data$Season, (s - numSeas):(s-1)) )
        } else {
          trainIndex = ((data$Season == s) & (data$Week < w & data$Week >= min(weeks))) | ( is.element(data$Week, weeks) & is.element(data$Season, (s - numSeas):(s-1)) )
        }
        if (inclEarlierGamesWithAllInfo){
          if (w > maxK + 1){
            trainIndex = trainIndex | (is.element(data$Season, (s-numSeas):(s)) & is.element(data$Week, (maxK+1):(w-1)) )
          }
        }
        out[['train']][[counter]] = trainIndex
        out[['validate']][[counter]] = validIndex
      } else {
        stop('fixed number of training games not implemented')
      }
    }
    prevS = s
    prevW = w
  }
  return(out)
}

fitGbmReturnMeanWeekSummStats = function(data, features, ptSpread, folds, betFunc, delta, ...){
  MM = data[,c(features, 'HomeMargin')]
  counter = 1
  rmses = c()
  lineRmses = c()
  winpcts = c()
  for (weekIndex in 1:length(folds[[1]])){
    fit = gbm(HomeMargin ~ ., data = MM[folds[[1]][[weekIndex]],],
              distribution = 'gaussian', ...)
    pred = predict.gbm(fit, newdata = MM[folds[[2]][[weekIndex]], c('HomeMargin', features)], 
                       n.trees = numTrees)
    line = ptSpread[folds[[2]][[weekIndex]]]
    hMargin = MM[folds[[2]][[weekIndex]], 'HomeMargin']
    betOn = betFunc(-pred, line, delta)
    betWinner = ifelse(hMargin + line > 0, 'H', ifelse(hMargin + line == 0, 'Push', 'A'))
    tieIndex = betWinner == 'Push'
    weekWinPct = mean(na.exclude(betOn[!tieIndex] == betWinner[!tieIndex]))
    rmse = sqrt(mean((pred - hMargin)^2))
    lineRmse = sqrt(mean((-line - hMargin)^2))
    rmses[counter] = rmse
    lineRmses[counter] = lineRmse
    winpcts[counter] = weekWinPct
    counter = counter + 1
  }
  return(data.frame(rmse = rmses, lineRmse = lineRmses, winPct = winpcts))
}

betFunc = function(myLine, ptSpread, delta){
  homeAwayOrNA = ifelse(myLine - ptSpread <= -delta, 'H', 'A')
  return(homeAwayOrNA)
}
