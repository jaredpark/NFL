# # Jan 15th data generation
# Jan 23 data gen
dateString = 'Feb7'
# genFeatures will save new files, allDat_(date).Rout and gameInfo_(date).Rout
# it will also load in uneeded objects along with the saved files
source('loadAndCleanData.R') # loads dat file with most of the regressors (Jan 15)
# loads in needed functions for creating MM:
source('./funcs/modelMatrixFuncs.v3.R')
# settings:
K = 0:12
weeks = 2:17
seas = 2001:2011
MM = dataToMM(dat, gameInfo, K, weeks, seas, imputeMedAndReturn, featureOrganize, dataToFeatures, dateString)
MM = dataToMM(dat, gameInfo, K, weeks, seas, dropColWithNA, featureOrganize, dataToFeatures, dateString)
rm(K, seas, weeks, dat)
ptSpreadAdd = addWeekOfGameFeat(allDat$PointSpread, gameInfo, MM)
MM = cbind(MM, 'HomePtSpread' = ptSpreadAdd)
homeMarginAdd = addWeekOfGameFeat(allDat$Score - allDat$OpponentScore, gameInfo, MM)
MM = cbind(MM, 'HomeMargin' = homeMarginAdd)
rm(ptSpreadAdd, homeMarginAdd)
save(MM, file = paste(dataPath, 'MM_', dateString, '.Rout', sep = ''))
# this will load in the results for use until new data generation:
# 1/23 included 1991 to 2012 with NA's imputed with the column median
# 2/7 included 2001 to 2011 and excluded all columns that included an NA

# to add new features using data held in allDat, use ... in dataToMM to pass args to 
# featureOrganize func to exclude the first N columns of the df; cbind to attach to
# the loaded MM (Jan 15 MM took ~ 45 sec per season or about 21 minutes)



