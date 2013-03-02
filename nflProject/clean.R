dataPath = '~/projects/nfl/nflProject/data_objects/'
load(paste(dataPath, '85to2012data.Rout', sep = ''))

cleanDateString = 'Feb16'
sourceFilePath = './'
source(paste(sourceFilePath, 'load.R', sep = ''))
source(paste(sourceFilePath, 'funcs.R', sep = ''))
rm(sourceFilePath)

allFeatNames = colnames(allDat)

# names of columns by game or team or opponent:
oppFeatNames = allFeatNames[grep('Opponent(.+)', allFeatNames)]
teamFeatNames = gsub('Opponent(.+)', '\\1', oppFeatNames)
gameFeatNames = allFeatNames[!is.element(allFeatNames, c(oppFeatNames, teamFeatNames))]

# newData is allDat minus some columns while removedData contains the removed columns:
removedData = allDat[, gameFeatNames]
newData = allDat[, c(teamFeatNames, oppFeatNames)]

# dumping columns with only NAs:

toRemove = colnames(newData[,emptyColumnIndex(newData)]) # see funcs.R for emptyColumnIndex() info

temp = removeColumnsAndReturn(newData, removedData, toRemove)
newData = temp[[1]]
removedData = temp[[2]]

# changing from two columns with minutes and seconds for time of possession to one column with fractional minutes

TOP = round(allDat$TimeOfPosessionMinutes + allDat$TimeOfPosessionSeconds/60, 2)
toRemove = c('TimeOfPosessionMinutes',
             'TimeOfPosessionSeconds',
             'OpponentTimeOfPosessionMinutes',
             'OpponentTimeOfPosessionSeconds',
             'TimeOfPosession', 'OpponentTimeOfPosession')

temp = removeColumnsAndReturn(newData, removedData, toRemove)
newData = temp[[1]]
removedData = temp[[2]]

newData = addColumn(newData, TOP)
rm(TOP)

# # removing 3rd and 4th down conversions and attempts; keeping pct
# 
# toRemove = c('ThirdDownAttempts',
#              'ThirdDownConversions',
#              'FourthDownAttempts',
#              'FourthDownConversions',
#              'OpponentThirdDownAttempts',
#              'OpponentThirdDownConversions',
#              'OpponentFourthDownAttempts',
#              'OpponentFourthDownConversions')
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]

# # removing red zone and goal to go conversions and attempts; leaving pct
# 
# toRemove =   c('RedZoneConversions',
#                'RedZoneAttempts',
#                'OpponentRedZoneConversions',
#                'OpponentRedZoneAttempts')
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]
# 
# toRemove = colnames(newData[grep('GoalToGo', colnames(newData))])
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]

# # removing by-quarter scoring
# 
# toRemove = paste('ScoreQuarter', c(1:4), sep = '')
# toRemove = c(toRemove, 'ScoreOvertime')
# toRemove = c(toRemove, paste('Opponent', toRemove, sep = ''))
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]

# # removing number of touchdowns, replacing with % points from TD + extra pt

TdToScoreRatio = newData$Touchdown*7/(newData$Score+10^-10)
OppTdToScoreRatio = newData$OpponentTouchdown*7/(newData$OpponentScore+10^-10)

# toRemove = paste(c('', 'Opponent'), 'Touchdowns', sep = '')
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]
# newData = addColumn(newData, TdToScoreRatio)
# newData = addColumn(newData, OppTdToScoreRatio)
rm(TdToScoreRatio, OppTdToScoreRatio)

# # removing number of first down by type data
# 
# toRemove = paste('FirstDowns', c('ByPenalty', 'ByRushing', 'ByPassing'), sep = '')
# toRemove = c(toRemove, paste('Opponent', toRemove, sep = ''))
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]

# # removing all data about extra points
# 
# toRemove = colnames(newData[grep('ExtraPoint', colnames(newData))])
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]

# changing name of int columns for clarity

InterceptionsThrown = newData$PassingInterceptions
OpponentInterceptionsThrown = newData$OpponentPassingInterceptions
toRemove = paste(c('', 'Opponent'), 'PassingInterceptions', sep = '')

temp = removeColumnsAndReturn(newData, removedData, toRemove)
newData = temp[[1]]
removedData = temp[[2]]

newData = addColumn(newData, InterceptionsThrown)
newData = addColumn(newData, OpponentInterceptionsThrown)

rm(InterceptionsThrown, OpponentInterceptionsThrown)

# removing fumbles lost incl total num turnovers, keeping total number of fumbles; fumble recoveries are thought to be random; descriptive not predictive

toRemove = paste(c('', 'Opponent'), 'FumblesLost', sep = '')

temp = removeColumnsAndReturn(newData, removedData, toRemove)
newData = temp[[1]]
removedData = temp[[2]]

toRemove = paste(c('', 'Opponent'), 'Giveaways', sep = '')
toRemove = c(toRemove, paste(c('', 'Opponent'), 'Takeaways', sep = ''))

temp = removeColumnsAndReturn(newData, removedData, toRemove)
newData = temp[[1]]
removedData = temp[[2]]

toRemove = paste(c('', 'Opponent'), 'TurnoverDifferential', sep = '')

temp = removeColumnsAndReturn(newData, removedData, toRemove)
newData = temp[[1]]
removedData = temp[[2]]

# # removing penalty yards
# 
# toRemove = paste(c('', 'Opponent'), 'PenaltyYards', sep = '')
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]

# # removing sack yards
# 
# toRemove = paste(c('', 'Opponent'), 'TimesSackedYards', sep = '')
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]

# # removing safeties
# 
# toRemove = paste(c('', 'Opponent'), 'Safeties', sep = '')
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]

# removing punt yards

toRemove = paste(c('', 'Opponent'), 'PuntYards', sep = '')

temp = removeColumnsAndReturn(newData, removedData, toRemove)
newData = temp[[1]]
removedData = temp[[2]]

# # removing tackles for loss, yards
# 
# toRemove = paste(c('', 'Opponent'), 'TacklesForLossDifferential', sep = '')
# toRemove = c(toRemove, paste(c('', 'Opponent'), 'TacklesForLoss', sep = ''))
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]

# # removing times sacked
# 
# toRemove = paste(c('', 'Opponent'), 'TimesSacked', sep = '')
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]
# 
# toRemove = paste(c('', 'Opponent'), '', sep = 'QuarterbackSacksDifferential')
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]

# # removing qb hits
# 
# toRemove = paste(c('', 'Opponent'), 'QuarterbackHitsDifferential', sep = '')
# toRemove = c(toRemove, paste(c('', 'Opponent'), 'QuarterbackHits', sep = ''))
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]

# # removing passing info
# 
# toRemove = paste(c('', 'Opponent'), 'PassingYardsPerCompletion', sep = '')
# toRemove = c(toRemove, paste(c('', 'Opponent'), 'PassingCompletions', sep = ''))
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]

# # removing touchdowns by type
# 
# toRemove = paste(c('Passing', 'Rushing'), 'Touchdowns', sep = '')
# toRemove = c(toRemove, paste('Opponent', toRemove, sep = ''))
# 
# temp = removeColumnsAndReturn(newData, removedData, toRemove)
# newData = temp[[1]]
# removedData = temp[[2]]
# 
rm(allFeatNames, gameFeatNames, teamFeatNames, oppFeatNames, temp, toRemove)

# organization stuffs

infoNames = c('Season', 'Week', 'Team', 'Opponent', 'HomeOrAway')
infoIndex = is.element(colnames(removedData), infoNames)
gameInfo = removedData[, infoIndex]
unusedDat = removedData[, !infoIndex]
dat = newData
rm(removedData, newData, infoIndex, infoNames)

# must provide dateString:
save(gameInfo, file = paste(dataPath, 'gameInfo_', cleanDateString, '.Rout', sep = ''))
save(allDat, file = paste(dataPath, 'allDat_', cleanDateString, '.Rout', sep = ''))
# save(unusedDat, file = paste(dataPath, 'unusedDat_', cleanDateString, '.Rout', sep = ''))
rm(unusedDat, allDat, gameInfo)

# abbreviating column names for brevity
colnames(dat) = gsub('Opponent', 'Opp', colnames(dat))
colnames(dat) = gsub('Yards', 'Yds', colnames(dat))
colnames(dat) = gsub('Offensive', 'Off', colnames(dat))
colnames(dat) = gsub('Passing', 'Pass', colnames(dat))
colnames(dat) = gsub('Rushing', 'Rush', colnames(dat))
colnames(dat) = gsub('Percentage', 'Pct', colnames(dat))
colnames(dat) = gsub('Quarterback', 'Qb', colnames(dat))
colnames(dat) = gsub('Interceptions', 'Int', colnames(dat))
colnames(dat) = gsub('Attempt', 'Att', colnames(dat))
colnames(dat) = gsub('Completion', 'Comp', colnames(dat))
colnames(dat) = gsub('Touchdowns', 'Td', colnames(dat))
colnames(dat) = gsub('Firstdowns', 'Fd', colnames(dat))
colnames(dat) = gsub('Average', 'Avg', colnames(dat))
colnames(dat) = gsub('Tackles', 'Tack', colnames(dat))
colnames(dat) = gsub('Passer', 'Qb', colnames(dat))
colnames(dat) = gsub('Per', 'P', colnames(dat))

# generating game result feature
gameResult = ifelse(dat$Score > dat$OppScore, 1, 
                    ifelse(dat$Score == dat$OppScore, .5, 0))
dat$GameResult = gameResult
rm(gameResult)

# generating margin of victory feature
Margin = dat$Score - dat$OppScore
dat$Margin = Margin
rm(Margin)

# excluding redundant opponent columns; each team has it's own row for each game
dat = dat[, -grep('^Opp', colnames(dat))]

save(dat, file = paste(dataPath, 'cleanData_', cleanDateString, '.Rout', sep = ''))

rm(dataPath, cleanDateString)