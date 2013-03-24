fitWindowSize = 40 # to be passed in through a bash script (loop over window sizes)

colExclude = unique(c(seq(6, 3485, by = 15), seq(8, 3485, by = 15),
                      seq(9, 3485, by = 15), seq(11, 3485, by = 15),
                      seq(12, 3485, by = 15), seq(13, 3485, by = 15),
                      seq(14, 3485, by = 15), seq(15, 3485, by = 15),
                      seq(16, 3485, by = 15), seq(17, 3485, by = 15),
                      seq(18, 3485, by = 15), seq(19, 3485, by = 15)))

#load MM
MMdateString = 'Mar18'
MMpath = '~/GitHub/NFL/nflProject/data_objects/'
load(paste(MMpath, 'MM_', MMdateString, '.Rout', sep = ''))
require(gbm)
source('~/GitHub/NFL/nflProject/funcs.R')

# configuration settings
numTrees = seq(250, 500, by = 250)
maxNumTrees = max(numTrees)
interactionDepth = seq(1, 2, by = 1)
minObsPerNode = seq(10, 10, by = 5)

gbmConfigMatrix = expand.grid('intDepth' = interactionDepth, 'minObs' = minObsPerNode)

validateWeeks = 2:16
validateSeas = 1990:2009
folds = MMFoldIndexList(MM, validateWeeks, validateSeas, numWeeks = fitWindowSize)
vWeeks = 1:length(folds[[1]])

settingsTable = NULL
# settingsTable is a matrix with p columns for each parameter and C rows for each combination of parameters
vWeekList = list()
# vWeekList has V elements, corresponding to each week in the range of validation data
  # each of the V elements is a list with C elements, corresponding to one unique combination of parameters; to one row in settingsTable
    # each of the C elements is a list with 3 elements: model, predictions, rmse
for (validationWeek in vWeeks){ # this loop handles tasks described in 'jon 3_20 notes' under 'script 1' and 'script 2'
  vWeekList[[validationWeek]] = list()
  configurationCounter = 0
  for (configRow in 1:nrow(gbmConfigMatrix)){
    settings = gbmConfigMatrix[configRow, ]
    attach(settings)
    data = MM[folds[[1]][[validationWeek]], -c(1:4, colExclude)]
    data = dropColWithNA(data, 'all')
    print(paste('validation week ', validationWeek, sep = ''))
    model = gbm(Response ~ ., distribution = 'gaussian', data = data, 
                n.trees = maxNumTrees, interaction.depth = intDepth, n.minobsinnode = minObs)
    for (trees in numTrees){
      configurationCounter = configurationCounter + 1
      predictions = predict.gbm(model, MM[folds[[2]][[validationWeek]], -c(1:4)], n.trees = trees)
      rmse = sqrt(mean((MM$Response[folds[[2]][[validationWeek]]] - predictions)^2))
      outputs = list(model, predictions, rmse)
      vWeekList[[validationWeek]][[configurationCounter]] = outputs
      if (validationWeek == min(vWeeks)){settingsTable = rbind(settingsTable, cbind('numTrees' = trees, 'intDepth' = intDepth, 'minObs' = minObs))}
    }
  }
}

summStatLocation = 3 # this is here in case changes are made to vWeekList organization, is used to reference rmse drawerin the following loop:
validtnWindowSizes = seq(15, 45, 15)
validtnWindowCounter = 1
bestConfigByValidationWindowSize = list()
for (validtnWindowSize in validtnWindowSizes){ # looping over u in workflow
  print(validtnWindowSize)
  finalPredWeeks = (validtnWindowSize + 1):(length(vWeekList))
  bestConfigurationByFinalPredWeek = c()
  bestModelByFinalPredWeek = list()
  finalPredWeekCounter = 1
  for (finalPredWeek in finalPredWeeks){ # looping over columns of merged validation results (blue)
    validtnWindowRange = (finalPredWeek - validtnWindowSize):(finalPredWeek-1)
    summStatByModelForFinalPredWeek = c()
    
    for (modelConfiguration in 1:nrow(settingsTable)){ # looping through rows of merged validation results
      weekCounter = 1
      summStatByValidtnWeek = c()
      for (validtnWeek in validtnWindowRange){ # looping across one row of merged validation results, for columns in validation window range (function of which final prediction week and size of validation window)
        summStatByValidtnWeek[weekCounter] = vWeekList[[validtnWeek]][[modelConfiguration]][[summStatLocation]]
        weekCounter = weekCounter + 1
      }
      
      summStatByModelForFinalPredWeek[modelConfiguration] = mean(summStatByValidtnWeek)
    }
    
    bestConfigurationByFinalPredWeek[finalPredWeekCounter] = which.min(summStatByModelForFinalPredWeek)
    bestModelByFinalPredWeek[[finalPredWeekCounter]] = vWeekList[[finalPredWeek]][[bestConfigurationByFinalPredWeek]]
    finalPredWeekCounter = finalPredWeekCounter + 1
  }
  bestConfigByValidationWindowSize[[validtnWindowCounter]] = bestConfigurationByFinalPredWeek
  bestModelByValidationWindowSize[[validtnWindowCounter]] = bestModelByFinalPredWeek
  validtnWindowCounter = validtnWindowCounter + 1
}

validtnWindowSizeMrmse = c()
for (validtnWindowCounter 1:length(bestModelByValidationWindowSize)){
  finalPredWeekRmse = c()
  for (finalPredWeekCounter in 1:length(bestModelByValidationWindowSize[[validtnWindowCounter]])){
    finalPredWeekRmse[finalPredWeekCounter] = bestModelByValidationWindowSize[[validtnWindowCounter]][[summStatLocation]]  
  }
  validtnWindowSizeMrmse[validtnWindowCounter] = mean(finalPredWeekRmse)  
}

bestBaseModelMachine = bestModelByFinalPredWeek[[which.min(validtnWindowSizeMrmse)]]
