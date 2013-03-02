emptyColumnIndex = function(data){
  # takes a data frame or matrix and returns a numeric index of columns with only NAs
  out = c()
  for (col in 1:ncol(data)){
    if (sum(is.na(data[,col])) == nrow(data)){
      out = c(out, col)
    }
  }
  return(out)
}

removeColumnsAndReturn = function(data, removedData, namesOfColumnsToExclude){
  # data is a named data frame containing the data that is to have a column removed and returned
  # removedData is a data frame that will be added to the removed data and returned
  # the output is a list of length 2 with the first element containing data minus the excluded
  # columns and the second element a data frame that combines removedData with the columns excluded
  newData = data[, !is.element(colnames(data), namesOfColumnsToExclude)]
  # is true if the column name is not the same as a column name already contained in removedData:
  addToRemovedIndex = !is.element(namesOfColumnsToExclude, colnames(removedData))
  newRemovedData = cbind(removedData, data[, is.element(colnames(data), namesOfColumnsToExclude[addToRemovedIndex])])
  return(list(newData, newRemovedData))
}

addColumn = function(data, vector){
  # takes a named df and a vector and returns a df containing both the initial df and the vector,
  # unless the name of the vector is shared with the name of a column of the initial df.
  if (length(vector)!=nrow(data)){
    stop('data must have same number of rows as length of vector')
  }
  toAdd = data.frame(...)
  isNew = !is.element(colnames(toAdd), colnames(data))
  if (isNew){
    out = cbind(data, ...)
    return(out)
  } else {
    print('New column name is duplicate with column in data')
    return(data)
  }
}

##############################
##### End clean data  ########
##############################

addWeekOfGameFeat = function(featureVec, gameInfo, MM){
  MMseas = unique(MM$Season)
  MMweeks = unique(MM$Week)
  MMindex = is.element(gameInfo$Season, MMseas) & is.element(gameInfo$Week, MMweeks) & gameInfo$HomeOrAway == 'HOME'
  gameData = gameInfo[MMindex, ]
  feature = featureVec[MMindex]
  out = NULL
  for (MMrow in 1:nrow(MM)){
    correspondingDataRow = which(gameData$Team == MM$Home[MMrow] & gameData$Week == MM$Week[MMrow] & gameData$Season == MM$Season[MMrow])
    out = c(out, feature[correspondingDataRow])
  }
  return(out)
}

dataToMM = function(data, gameInfo, K, trainWeeks, trainSeas, imputeFunc, featureOrgFunc, dataToFeatFunc, dateString, ...){
  data = imputeFunc(data)
  out = featureOrgFunc(dataToFeatFunc(data, gameInfo, K, trainWeeks, trainSeas), ...)
  save(out, file = paste('MM_', dateString, '.Rout', sep = ''))
  return(out)
}

featureOrganize = function(features, excludeFirstNColumns = F, N = 4){
  print('Model matrix organization BEGIN')
  seasons = sort(unique(features$Season))
  weeks = sort(unique(features$Week))
  outData = NULL
  for (seas in seasons){
    seasData = NULL
    for (week in weeks){
      weekData = features[features$Season == seas & features$Week == week, ]
      homeData = weekData[weekData$HomeOrAway == 'HOME', ]
      colnames(homeData)[6:ncol(homeData)] = paste('Home', colnames(homeData)[6:ncol(homeData)], sep = '')
      awayData = weekData[weekData$HomeOrAway == 'AWAY', ]
      colnames(awayData)[6:ncol(awayData)] = paste('Away', colnames(awayData)[6:ncol(awayData)], sep = '')
      orderedHomeData = homeData[order(homeData$Team), ]
      matchingAwayData = awayData[order(awayData$Opponent), ]
      completeWeekData = cbind(orderedHomeData, matchingAwayData[,-(1:5)])
      
      seasData = rbind(seasData, completeWeekData)
    }
    outData = rbind(outData, seasData)
  }
  colnames(outData)[is.element(colnames(outData), c('Team', 'Opponent'))] = c('Home', 'Away')
  removeCol = which(colnames(outData) == 'HomeOrAway')
  outData = outData[, -removeCol]
  if (excludeFirstNColumns){
    print('Model matrix organization END')
    return(outData[, -c(1:N)])
  } else {
    print('Model matrix organization END')
    return(outData)
  }
}

dropColWithNA = function(data, ...){
  hasNA = as.logical(apply(data, 2, sumNA))
  out = data[, !hasNA]
  return(out)
}

sumNA = function(vector){
  out = sum(is.na(vector))
  return(out)
}

imputeMedAndReturn = function(data, ...){
  require(randomForest)
  out = na.roughfix(data)
  return(out)
}

dataToFeatures = function(data, gameInfo, K, trainWeeks, trainSeasons){
  # takes nfldata.com team/opponent style data and returns features
  # features are the sum and/or average of the previous K weeks of data
  # each row data for one team prior to one game during the training region. each column
  # is the k week sum or average of some datum contained in data
  if (!nrow(data)==nrow(gameInfo)){ stop('data and gameInfo must have the same number of rows') }
  out = NULL
  for (seas in trainSeasons){
    print(paste('Beginning', seas, 'season'))
    seasonData = seasonHelper(data[gameInfo$Season == seas & gameInfo$Week < max(trainWeeks),], 
                              gameInfo[gameInfo$Season == seas, ], 
                              K, trainWeeks)
    out = rbind(out, seasonData)
  }
  return(out)
}

seasonHelper = function(data, gameInfo, K, trainWeeks){
  # data and gameinfo should be for one given season
  teams = unique(gameInfo$Team)
  seasData = NULL
  for (team in teams){
    teamSeasData = data[gameInfo$Team == team, ]
    vsSeasData = data[gameInfo$Opponent == team, ]
    seasTeamGameInfo = gameInfo[gameInfo$Team == team, ]
    # returns a dataframe for one team with all datum in 'data' for each week in a season
    seasTeamFeats = NULL
    for (datum in 1:ncol(data)){
      datumName = colnames(data)[datum]
      toExpand = teamSeasData[, datum]
      vsToExpand = vsSeasData[, datum]
      seasTeamDatumFeat = NULL
      seasVsTeamDatumFeat = NULL
      for (week in trainWeeks){
        if (is.element(week, seasTeamGameInfo$Week)){
          seasWeekTeamDatumFeat = featureHelper(seasTeamGameInfo, toExpand, datumName, K, week, 'for', 'allWeek')
          # returns a vector length K with a given datum averaged over the last k weeks prior to the given week
          seasTeamDatumFeat = rbind(seasTeamDatumFeat, seasWeekTeamDatumFeat)
          # df with length(K) columns and length(trainWeeks) rows
          seasWeekVsTeamDatumFeat = featureHelper(seasTeamGameInfo, vsToExpand, datumName, K, week, 'vs', 'allWeek')
          seasVsTeamDatumFeat = rbind(seasVsTeamDatumFeat, seasWeekVsTeamDatumFeat)
        }
      }
      seasTeamFeats = cbind(seasTeamFeats, seasTeamDatumFeat, seasVsTeamDatumFeat)
    }
    seasTeamData = cbind(seasTeamGameInfo[is.element(seasTeamGameInfo$Week, trainWeeks), ], seasTeamFeats)
    seasData = rbind(seasData, seasTeamData)
  }
  return(seasData)
}

featureHelper = function(seasTeamGameInfo, toExpand, datumName, K, week, forOrVs = 'for', largeKaction = 'allWeek'){
  # takes a vector of weekly observations of a datum for a team in a season
  # returns a vector of length length(K) with each entry holding the k week
  # average of the datum over the previous weeks for the given team, season, week
  expandedDatum = c()
  counter = 1
  for (k in K){
    if (k == 0){
      # stand in for all season data
      prevKWeeksData = toExpand[seasTeamGameInfo$Week < week]
    } else{
      if (min(seasTeamGameInfo$Week) == week){
        prevKWeeksData = NA
      } else{
        endRow = which.max(seasTeamGameInfo$Week[seasTeamGameInfo$Week < week])
        prevSeasGames = seasTeamGameInfo$Week[1:endRow]
        if (length(prevSeasGames) < k){
          if (largeKaction == 'NA'){
          prevKWeeksData = NA
          } else if (largeKaction == 'allWeek'){
            prevKWeeksData = toExpand[seasTeamGameInfo$Week < week]
          }
        } else{
          startRow = endRow - k + 1
          prevKWeeksData = toExpand[startRow:endRow]
        }
      }
    }
    expandedDatum[counter] = mean(prevKWeeksData)
    # each entry is a datum averaged over the previous k weeks
    counter = counter + 1
  }
  prettyK = ifelse(K == 0, 'All', K)
  names(expandedDatum) = paste(datumName, ifelse(forOrVs == 'for', '', 'Vs'), prettyK, 'Wk', sep = '')
  
  return(expandedDatum)
}

##############################
######## End make MM  ########
##############################

MMFoldIndexList = function(MM, numWeeksFixed = F, weeks, seasons, numSeas, numWeeks, 
                           numLaterGames = 'all', trainWithWk17 = F, inclEarlierGamesWithAllInfo, maxK = NULL){
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
  MMindex = which(is.element(data$Season, seasons) & is.element(data$Week, weeks))
  prevS = 0
  prevW = 0
  counter = 0
  for (row in MMindex){
    s = data[row, 'Season']
    w = data[row, 'Week']
    if (s == prevS & w == prevW){
      # do nothing, already have the train/valid indeces stored in out
    } else {
      counter = counter + 1
      validIndex = data$Week == w & data$Season == s
      out[['validate']][[counter]] = validIndex
      if (!numWeeksFixed){
        # if not fixed, this method uses previous x seasons games held in weeks, in addition to all prev
        # in-season games held in week; training a rule for validation on a given week games
        trainIndex = ((data$Season == s) & (data$Week < w & data$Week >= min(weeks)))
        # in season previous week games within the range of weeks
        last = ifelse(trainWithWk17, 17, 16)
        if (numLaterGames == 'all'){
          trainIndex = trainIndex | (is.element(data$Season, (s - numSeas):(s-1)) & is.element(data$Week, min(weeks):last))
        } else if (is.numeric(numLaterGames)){
          if (max(w)+numLaterGames > last){
            trainIndex = trainIndex | (is.element(data$Season, (s - numSeas):(s-1)) & is.element(data$Week, min(weeks):last))
          } else {
            trainIndex = trainIndex | (is.element(data$Season, (s - numSeas):(s-1)) & is.element(data$Week, min(weeks):(max(weeks)+numLaterGames)))
          }
        }
        if (inclEarlierGamesWithAllInfo){
          if (w > maxK + 1){
            trainIndex = trainIndex | (is.element(data$Season, (s-numSeas):(s)) & is.element(data$Week, (maxK+1):(w-1)) )
          }
        }
      } else{
        # number of weeks of training data is fixed; all validation rolls contain games from the same number of weeks, though not always the same number of total games
        trainIndex = ((data$Season == s) & (data$Week < w & data$Week >= min(weeks)))
        currNum = 
        last = ifelse(trainWithWk17, 17, 16)
#        
#         
#         
#         
#         should the validation window include games from weeks not used to validate the model?
#         
#         
#         
#         
#         
        
        trainIndex = trainIndex | 
      }
      
      out[['train']][[counter]] = trainIndex
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

##############################
####### End validation #######
##############################

