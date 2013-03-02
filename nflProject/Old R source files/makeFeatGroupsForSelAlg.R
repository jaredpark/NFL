load('training.data.Nov1.Rout')
# source('project.MM.v3.R')

push.index = which(is.na(train.dat$response))
train.dat = train.dat[-push.index, ]
N = nrow(train.dat)

numSeas = length(unique(train.dat$SEAS))
folds = list()
for (i in seq(0, numSeas-1)){
  folds[[i+1]] = which(train.dat$SEAS == 2000 + i)
}

cv.dat = train.dat[, -c(2:6)]
sc.cv.dat = cv.dat
sc.cv.dat[,-1] = scale(cv.dat[, -1])

R = ncol(sc.cv.dat) - 1
corr = c()
for (col in 1:R){
  x = sc.cv.dat[, col+1]
  index = !is.na(x)
  x = x[index]
  y = as.numeric(sc.cv.dat$response[index]) - 1
  corr[col] = cor(x, y)
}
summary(corr^2)

always.include = c(2)
R = ncol(cv.dat) - 1 - length(always.include)
solo.acc = c()
for (col in 1:(R)){
  columns = c(1, always.include, col + 1 + length(always.include))
  accuracy = c()
  for (fold in 1:length(folds)){
    model.dat = cv.dat[(1:N)[-folds[[fold]]], columns]
    valid.dat = cv.dat[folds[[fold]], columns]
    fit = glm(response ~ ., data = model.dat, family = binomial(link = 'logit'))
    pred = predict.glm(object = fit, newdata = valid.dat, type = 'response')
    pred.class = as.numeric(pred[!is.na(pred)] >= .5)
    actual.class = as.numeric(valid.dat$response[!is.na(pred)]) - 1
    accuracy[fold] = mean(pred.class == actual.class)
  }
  solo.acc[col] = mean(accuracy)
}
summary(solo.acc)

# nGen = R
# test.accuracy = c()
# test.corr = c()
# y = as.numeric(cv.dat$response) - 1
# cv.accuracy = c()
# for (i in 1:nGen){
#   test = rnorm(N, 0, 1)
#   test.corr[i] = cor(test, y)
#   accuracy = c()
#   datata = data.frame(y, test)
#   for (f in 1:length(folds)){
#     t.datata = datata[(1:N)[-folds[[f]]], ]
#     v.datata = datata[folds[[f]], ]
#     fit = glm(y ~ ., data = t.datata, family = binomial(link = 'logit'))
#     pred = predict.glm(object = fit, newdata = v.datata, type = 'response')
#     pred.class = as.numeric(pred[!is.na(pred)] >= .5)
#     actual.class = v.datata$y
#     accuracy[f] = mean(pred.class == actual.class)
#   }
#   test.accuracy[i] = mean(accuracy)
# }
# summary(test.corr^2)
# summary(test.accuracy)
N = ncol(cv.dat)
R = ncol(cv.dat) - 1 - length(always.include)

acc.exclude.portion = .8
acc.cut.point = ceiling(R*acc.exclude.portion)
best.acc = order(solo.acc)[acc.cut.point:R] + 1 + length(always.include)

corr.exclude.portion = .8
corr.cut.point = ceiling(R*corr.exclude.portion)
best.corr = order(corr)[corr.cut.point:R]  + 1

others = (3:N)[!is.element(3:N, c(best.corr, best.acc))]
num.to.add = length(best.corr) - length(others)

if (num.to.add>0){
  to.add = sample((3:N)[-others], size = num.to.add, replace = F)
  others = c(others, to.add)
}

feature.groups = list(acc = names(cv.dat[unique(c(1, always.include, best.acc))]), 
                      corr = names(cv.dat[unique(c(1, always.include, best.corr))]), 
                      other = names(cv.dat[unique(c(1, always.include, others))]))

save(feature.groups, file = 'featureGroups.Rout')

feature.groups[['acc.no.RZP']] = feature.groups[[1]][-c(7, 31, 39, 45, 53)]
feature.groups[['corr.no.RZ']] = feature.groups[[2]][-c(6, 12, 20, 30, 35, 36, 42, 49, 52, 53, 54)]