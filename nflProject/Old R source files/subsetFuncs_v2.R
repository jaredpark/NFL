MMFoldIndexList = function(MM, numWeeksFixed = F, weeks, seasons, numSeas, numWeeks){
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
        # if not fixed, this method uses previous x seasons data in addition to all prev
        # in-season data for training a rule for validation on a given week games
        # ... is thus the number of seasons to use
        trainIndex = is.element(data$Week, weeks) & ( is.element(data$Season, (s - numSeas):(s-1)) | ((data$Season == s) & (data$Week < w)) )
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

excludeFeatFunc = function(allFeatureNames, weeks){
  # excludes all features with k - 2 > min(weeks) - can implement other methods later
  minWeek = min(weeks)
  minToInclude = minWeek - 2
  if (minToInclude < 10){
    index = grep(paste('[^0-9][1-', minToInclude, '][^0-9]', sep = ''), allFeatureNames)
  } else {
    early = grep(paste('[^0-9][1-9][^0-9]', sep = ''), allFeatureNames)
    late = grep(paste('[^0-9]1[0-', minToInclude - 10, '][^0-9]'))
    index = c(early, late)
  }
  index = c(index, grep('AllWk', allFeatureNames))
  out = allFeatureNames[index]
  return(out)
}

subsetSizeFunc = function(sizeScores = 'uniform', makeNewSubset, prevSubset, min, max, mean, var){
  # currently only implemented for 'default' settings
  # returns a number; subset size sampling
  if (makeNewSubset){
    if (sizeScores == 'uniform'){
      out = sample(min:max, 1)
    } else{
      stop(paste('sizeScores =', sizeScores, 'not implemented.'))
    }
  } else {
    prevNum = length(prevSubset)
    # can implement prior based on sizescores or sampling
    noise = rnorm(1, 0, 1)
    sign = sign(noise)
    change = floor(abs(noise))
    newNum = prevNum + change*sign
    out = newNum
  }
  return(out)
}

perturbFunc = function(allFeatureNames, newSubsetSize, makeNewSubset, prevSubset, featureScores = NULL){
  if (length(featureScores)){stop('featureScores not implemented')}
  featureProbs = featureScores
  if (makeNewSubset){
    newSubset = sample(allFeatureNames, newSubsetSize, prob = featureProbs)
  } else{
    change = length(prevSubset) - newSubsetSize
    absChange = abs(change)
    if (change > 0){
      new = sample(allFeatureNames[!is.element(allFeatureNames, prevSubset)], absChange, prob = featureProbs)
      newSubset = c(prevSubset, new)
    } else if (change < 0){
      remove = sample(prevSubset, absChange, prob = featureProbs)
      newSubset = prevSubset[!is.element(prevSubset, remove)]
    } else if (change == 0) {
      numToChange = sample(1:2, 1, prob = c(.75, .25))
      remove = sample(prevSubset, numToChange)
      new = sample(allFeatureNames[!is.element(allFeatureNames, prevSubset)], numToChange)
      newSubset = c(prevSubset[!is.element(prevSubset, remove)], new)
    }
  }
  return(newSubset)
}

subsetCheck = function(subset, allPrevSubsets, docLoc){
  if (length(docLoc)){stop('file read for previous subsets not implemented')}
  if (length(allPrevSubsets)){
    isUniqueSubset = !as.logical(sum(apply(allPrevSubsets, 1, setequal, subset)))
  } else {
    isUniqueSubset = T
  }
  return(isUniqueSubset)
}

genFeatSubset = function(MM, weeks, makeNewSubset = F, prevSubset = NULL, excludeByName, subsetSizeFunc, 
                         perturbFunc, excludeFeatFunc, minNfeats = NULL, maxNfeats = NULL, 
                         allPrevSubsets = NULL, featureScores = NULL, sizeScores = 'uniform', 
                         docsLoc = NULL, meanNfeats = NULL, varNfeats = NULL){
  
  allFeatureNames = colnames(MM)[!is.element(colnames(MM), excludeByName)]
  allFeatureNames = excludeFeatFunc(allFeatureNames, weeks)
  
  uniqueSubset = F
  while (!uniqueSubset){
    newSubsetSize = subsetSizeFunc(sizeScores, makeNewSubset, prevSubset, minNfeats, maxNfeats, meanNfeats, varNfeats)
    subset = perturbFunc(allFeatureNames, newSubsetSize, makeNewSubset, prevSubset, featureScores)
    uniqueSubset = subsetCheck(subset, allPrevSubsets, docsLoc)
  }
  return(subset)
}