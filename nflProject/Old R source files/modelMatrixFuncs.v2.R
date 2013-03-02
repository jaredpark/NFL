datToMM = function(data, gameInfo, K, trainWeeks, trainSeasimputeFunc, featureOrgFunc, dataToFeatFunc, dateString){
  data = imputeFunc(data)
  out = featureOrgFunc(dataToFeatFunc(data, gameInfo, K, trainWeeks, trainSeas))
  save(out, file = paste('MM_', dateString, '.Rout', sep = ''))
  return(out)
}
  
  

featureOrganize = function(features){
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

  return(outData)
}

imputeMedAndReturn = function(data, ...){
  require(randomForest)
  out = na.roughfix(data)
  return(out)
}

dataToFeatures = function(data, gameInfo, K, trainWeeks, trainSeasons, inclGameInfo = T){
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
                              K, trainWeeks, inclGameInfo)
    out = rbind(out, seasonData)
  }
  return(out)
}

seasonHelper = function(data, gameInfo, K, trainWeeks, inclGameInfo){
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
            seasWeekTeamDatumFeat = featureHelper(seasTeamGameInfo, toExpand, datumName, K, week)
            # returns a vector length K with a given datum averaged over the last k weeks prior to the given week
            seasTeamDatumFeat = rbind(seasTeamDatumFeat, seasWeekTeamDatumFeat)
            # df with length(K) columns and length(trainWeeks) rows
            seasWeekVsTeamDatumFeat = featureHelper(seasTeamGameInfo, vsToExpand, datumName, K, week, 'vs')
            seasVsTeamDatumFeat = rbind(seasVsTeamDatumFeat, seasWeekVsTeamDatumFeat)
          }
        }
      seasTeamFeats = cbind(seasTeamFeats, seasTeamDatumFeat, seasVsTeamDatumFeat)
    }
    if (inclGameInfo){
      seasTeamData = cbind(seasTeamGameInfo[is.element(seasTeamGameInfo$Week, trainWeeks), ], seasTeamFeats)
    } else{
      seasTeamData = cbind(seasTeamFeats)
    }
    seasData = rbind(seasData, seasTeamData)
  }
  if (inclGameInfo){
    return(seasData)
  } else{
    return(as.data.frame(seasData))
  }
}

featureHelper = function(seasTeamGameInfo, toExpand, datumName, K, week, forOrVs = 'for'){
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
      endRow = which.max(seasTeamGameInfo$Week[seasTeamGameInfo$Week < week])
      prevSeasGames = seasTeamGameInfo$Week[1:endRow]
      if (length(prevSeasGames) < k){
        prevKWeeksData = NA
      } else{
        startRow = endRow - k + 1
        prevKWeeksData = toExpand[startRow:endRow]
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

# excludeVs = c('TOP', 'GameResult', 'Margin', 'SqrtMargin')