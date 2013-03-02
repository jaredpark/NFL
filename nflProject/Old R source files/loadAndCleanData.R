rawDataPath = '~/projects/nfl/85 to 11 data/'
saveToPath = '~/projects/nfl/nflProject/data_objects/'
source('./funcs/cleanDataFuncs.R')

dat = read.csv(file = paste(rawDataPath,'TeamGame.1985-2011.csv', sep = ''))
rawDataPath = '~/projects/nfl/12 data/'
data2012 = read.csv(file = paste(rawDataPath,'TeamGame.2012.csv', sep = ''))
allDat = rbind(dat, data2012)

rm(rawDataPath)

