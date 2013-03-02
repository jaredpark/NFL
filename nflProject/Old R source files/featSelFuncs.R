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