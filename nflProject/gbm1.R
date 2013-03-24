modelList = list()
predList = list()
rmseVec = c()
for (validationWeek in 1:length(folds[[1]])){
  model = gbm(Response ~ ., data = MM[folds[[1]][[validationWeek]], -c(1:4)], 
              n.trees = maxNumTrees, interaction.depth = intDepth, n.minobsinnode = minObs)
  modelList[[counter]] = model
  for (trees in numTrees){
    configCounter = configCounter + 1
    predictions = predict.gbm(model, MM[folds[[2]][[validationWeek]], -c(1:4)], n.trees = trees)
    rmse = sqrt(mean((MM$Response[folds[[2]][[validationWeek]]] - predictions)^2))
    predList[[validationWeek]] = predictions
    rmseVec[validationWeek] = rmse
    
  }
}
mrmse = mean(rmseVec)

save(modelList, file = '')
save(predList, file = '')
save(mrmse, file = '')