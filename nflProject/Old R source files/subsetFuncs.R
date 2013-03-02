# generate feature subset function:
# outputs a character vector of MM column names
# will create an initial subset or perturb an existing subset - logic arg AND optional vector arg
# will take a MM with columns optionally excluded by column name vector - vector arg
# will be robust to:
#   choice of distribution for subset size - function arg
#   perturbation scheme - function arg
#   inclusion/exclusion of features based on k and weeks - function arg 
# will write and read external documents for:
#   feature subset uniqueness compared to all previous iterations (parallel included) - document arg
#   feature specific scores; perturbation scheme add/remove decisions - document arg
#   feature subset size scores; perturbation scheme subset size decision - document arg

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