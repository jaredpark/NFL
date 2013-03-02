# source('project.MM.v3.R')

load('training.data.Nov1.Rout')
load('featureGroups.Rout')

push.index = which(is.na(train.dat$response))
train.dat = train.dat[-push.index, ]
N = nrow(train.dat)
which.feature.group = 'nov5.small.group'
small.group.of.features = T
always.include = c(2)

cv.dat = train.dat[, -c(2:6)]
cv.dat = cv.dat[, feature.groups[[which.feature.group]]]
# for second pass
#cv.dat = cv.dat[, c(1, unique(included)+1)]

LR = T
GBM = F
if (GBM){cv.dat$response = as.numeric(cv.dat$response) - 1; require(gbm)}
if (LR) {require(boot)}

# Season based CV
numSeas = length(unique(train.dat$SEAS))
folds = list()
for (i in seq(0, numSeas - 1)){
  folds[[i+1]] = which(train.dat$SEAS == 2000 + i)
}
tune.years = 1:(numSeas) # use to define which of the F fold accuracies are averaged; 1:F means all are averaged, while 6:F with F = 9 would average only the last 4 years to provide the model's cv.accuracy score

R = ncol(cv.dat) - 1 - length(always.include)

p = .6
min.subset = 6
plot(density(replicate(5000, sum(rbinom(n = 1, size = R, p)))))
print(paste(R*p, 'expectation per subset,', min.subset, 'minimum'))

curr.length = 0
while (curr.length < min.subset){
  curr.subset = which(as.logical(replicate(R, rbinom(n = 1, size = 1, p)))) + 1 + length(always.include)
  curr.length = length(curr.subset)
}
feature.set = list()
best.subset = list()
b = 1
best.subset[[b]] = curr.subset
cv.accuracy = c(.5)
K = 500
k = 1
while (k <= K){
  feature.set[[k]] = unique(c(always.include, curr.subset))
  accuracy = c()
  for (f in 1:length(folds)){
    model.dat = cv.dat[(1:N)[-folds[[f]]], unique(c(1, feature.set[[k]]))]
    valid.dat = cv.dat[folds[[f]], unique(c(1, feature.set[[k]]))]
    if (LR){
      fit = glm(response ~ 1 + ., data = model.dat, family = binomial(link = 'logit'))
      pred = predict.glm(object = fit, newdata = valid.dat, type = 'response')
      pred.class = as.numeric(pred[!is.na(pred)] >= .5)
      actual.class = as.numeric(valid.dat$response[!is.na(pred)]) - 1
    } else if (GBM){
      fit = gbm(response ~ ., data = model.dat, cv.folds = 5, shrinkage = 0.01, verbose = F,
                n.trees = 40, interaction.depth = 2, n.minobsinnode = 10, distribution = 'bernoulli')
      best.iter = gbm.perf(fit, method = 'cv')
      pred = predict.gbm(fit, valid.dat, n.trees = best.iter)
      pred.class = as.numeric(pred[!is.na(pred)] >= .5)
      actual.class = as.numeric(valid.dat$response[!is.na(pred)])
    }
    accuracy[f] = mean(pred.class == actual.class)
  }
  cv.accuracy[k] = mean(accuracy[tune.years])
  if (cv.accuracy[k] <= .5){
    is.duplicate.subset = T
    while (is.duplicate.subset){
      is.duplicate.subset = F
      
      curr.length = 0
      while (curr.length <= min.subset){
        curr.subset = which(as.logical(replicate(R, rbinom(n = 1, size = 1, p)))) + 1 + length(always.include)
        curr.length = length(curr.subset)
      }
      curr.subset = unique(c(2, curr.subset))
      
      for (i in 1:k){
        if (length(feature.set[[i]]) == length(curr.subset)){
          if (mean(is.element(feature.set[[i]], curr.subset)) == 1)
            is.duplicate.subset = T
        }
      }
    }
    
  } else { # the accuracy of the classifier is better than 50%
    if (k != 1){
      if (cv.accuracy[k] > max(cv.accuracy[-k])){
        best.subset[[b+1]] = curr.subset
        b = b + 1
      }
    }
    is.duplicate.subset = T
    while (is.duplicate.subset){
      is.duplicate.subset = F
      p.switch = exp((cv.accuracy[k]-.5)/3)/36
      perturb = c()
      while (length(perturb) == 0){
        if (small.group.of.features){
          perturb = sample(length(curr.subset), 1)
        }
        perturb = which(as.logical(replicate(curr.length, rbinom(1, 1, p.switch))))
      }
      n.new = length(perturb)
      new.features = sample((1:R)[-c(curr.subset)], size = n.new, replace = F) + 1 + length(always.include)
      curr.subset = unique(c(always.include, new.features, curr.subset[-perturb]))
      
      for (i in 1:k){
        if (length(feature.set[[i]]) == length(curr.subset)){
          if (mean(is.element(feature.set[[i]], curr.subset)) == 1)
            is.duplicate.subset = T
        }
      }
    }
  }
  if (k%%10 == 0){
    print(k)
    print(paste('curr acc is', round(cv.accuracy[k], 4), 'and max is', round(max(cv.accuracy), 4)))
    print(sort(curr.subset))
  }
  k = k + 1
}
plot(cv.accuracy, type = 'l')

all= c()
for (i in 1:length(feature.set)){
  all[i] = length(feature.set[[i]])
}
boxplot(cv.accuracy~all)
hist(all, freq = F, breaks = seq(5, 25, by = 1))
best = c()
n = 1
good = which(cv.accuracy > .54)
for (i in good){
  best[n] = length(feature.set[[i]])
  n = n + 1
}
hist(best, add = T, col = 2, fill = T, freq = F, , breaks = seq(5, 25, by = 1))

feat = c()
for (i in good){
  feat = c(feat, unlist(feature.set[[i]]))
}
length(table(feat))
c = 10
length(which(table(feat) >= c))

new.group = c(1, as.numeric(names(which(table(feat)>=c))))
new.group = best.subset[[length(best.subset)]]
# combined.group = unique(c(feature.groups[['corr.nov5']], feature.groups[['acc.nov5']]))
group.name = 'final.nov5'
# feature.groups[[group.name]] = combined.group
feature.groups[[group.name]] = feature.groups[[which.feature.group]][new.group]
# feature.groups[['all3']] = feature.groups[[which.feature.group]][c(1, as.numeric(names(which(table(feat)>=c))))]
# save.it.foo.3 = feature.groups[['all2']][c(1, best.subset[[length(best.subset)]])]
# feature.groups[['all2']] = feature.groups[[which.feature.group]][c(1, as.numeric(names(which(table(feat)>=c))))]
# feature.groups[['corr1']] = feature.groups[[which.feature.group]][c(1, as.numeric(names(which(table(feat)>=c))))]
# feature.groups[['acc1']] = feature.groups[[which.feature.group]][c(1, as.numeric(names(which(table(feat)>=c))))]
# feature.groups[['all1']] = unique(c(feature.groups[['corr1']], feature.groups[['acc1']]))
# # acc.feat = feature.groups[[which.feature.group]][c(1, as.numeric(names(which(table(feat)>=3))))]
# # corr.feat = feature.groups[[which.feature.group]][c(1, as.numeric(names(which(table(feat)>=3))))]
# # all.feat = unique(c(acc.feat, corr.feat))
# # feature.groups[['acc.corr.best']] = all.feat
# # trim.all.feat = feature.groups[[which.feature.group]][c(1, as.numeric(names(which(table(feat)>=4))))]
# # feature.groups[['trim.best']] = trim.all.feat
# # trim.all.feat.2 = feature.groups[[which.feature.group]][c(1, as.numeric(names(which(table(feat)>=2))))]
# # feature.groups[['trim.best.2']] = trim.all.feat.2
# # save.it.foo = best.subset[[9]] # 0.5591473
# trim.all.feat.2 = feature.groups[[which.feature.group]][c(1, as.numeric(names(which(table(feat)>=6))))]
# feature.groups[['trim.best.2']] = trim.all.feat.2
# trim.all.feat.3 = feature.groups[[which.feature.group]][c(1, as.numeric(names(which(table(feat)>=10))))]
# feature.groups[['trim.best.3']] = trim.all.feat.3
# feature.groups[['final']] = feature.groups[[which.feature.group]][c(1, best.subset[[4]])]
# feature.groups[['final2']] = feature.groups[[which.feature.group]][c(1, best.subset[[length(best.subset)]])]
# save(feature.groups, file = 'featureGroups.Rout')

