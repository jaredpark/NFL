emptyColumnIndex = function(data){
  out = c()
  for (col in 1:ncol(data)){
    if (sum(is.na(data[,col])) == nrow(data)){
      out = c(out, col)
    }
  }
  return(out)
}

firstSeasWithData = function(data){
  NAgrid = matrix(NA, nrow = length(unique(data$Season)), ncol = ncol(data))
  counter = 1
  for (seas in unique(data$Season)){
    NAgrid[counter, ] = apply(is.na(data[data$Season == seas, ]), 2, mean)
    counter = 1 + counter
  }
  out = rep(NA, ncol(data))
  for (feat in 1:ncol(data)){
    out[feat] = min(unique(data$Season)[NAgrid[,feat] == 0])
  }
  return(out)
}

noDataZeroToNA = function(data, gameInfo){
  out = data
  for (seas in unique(gameInfo$Season)){
    seasIndex = gameInfo$Season == seas
    seasDatumSums = apply(data[seasIndex, ], 2, sum)
    noDataAvailColIndex = which(seasDatumSums) == 0
    out[seasIndex, noDataAvailColIndex] = NA
  }
  return(out)
}

removeColumnsAndReturn = function(data, newData, namesOfColumnsToExclude){
  thinnedOutput = data[, !is.element(colnames(data), namesOfColumnsToExclude)]
  newTrimIndex = !is.element(namesOfColumnsToExclude, colnames(newData))
  trimmedOutput = cbind(newData, data[, is.element(colnames(data), namesOfColumnsToExclude[newTrimIndex])])
  return(list(thinnedOutput, trimmedOutput))
}

addColumn = function(data, ...){
  temp = data.frame(...)
  isNew = !is.element(colnames(temp), colnames(data))
  if (isNew){
    out = cbind(data, ...)
    return(out)
  } else {
    print('New column name is duplicate with column in data')
    return(data)
  }
}

