dataLoc = '~/projects/nfl/85 to 11 data/'
allDat = read.csv(file = paste(dataLoc,'TeamGame.1985-2011.csv', sep = ''))
dataLoc = '~/projects/nfl/12 data/'
data2012 = read.csv(file = paste(dataLoc,'TeamGame.2012.csv', sep = ''))
allDat = rbind(allDat, data2012)
rm(dataLoc, data2012)