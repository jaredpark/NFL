allGames = read.csv("gameindex.csv", T, ",")
source('fresh.funcs.v3.R')
### Add data to df
new = F

hMargin = allGames$PTS.H - allGames$PTS.V
hVsLineMargin = hMargin - allGames$SPR.V

dat = data.frame(allGames$GAME.ID, allGames$SEAS, allGames$WEEK, allGames$H, allGames$V, 
                 allGames$PTS.H, allGames$PTS.V, allGames$SPR.V)
names(dat) = c('GAME.ID', 'SEAS', 'WEEK', 'H', 'V', 'PTS.H', 'PTS.V', 'SPR.V')

dat$WIN.H = as.numeric(hMargin >= 0) # both teams get a win for ties
dat$WIN.V = as.numeric(hMargin <= 0)
dat$WWTS.H = as.numeric(hVsLineMargin > 0) # wins with the spread
dat$WWTS.V = as.numeric(hVsLineMargin < 0)
dat$LWTS.H = as.numeric(hVsLineMargin < 0) # wins with the spread
dat$LWTS.V = as.numeric(hVsLineMargin > 0)
dat$TWTS.H = as.numeric(hVsLineMargin == 0) # ties vs spread
dat$TWTS.V = as.numeric(hVsLineMargin == 0)
dat$MARGVS.H = hVsLineMargin # margin vs spread
dat$MARGVS.V = -hVsLineMargin
dat$PCP.H = allGames$PC.H/allGames$PA.H # pass completion percentage
dat$PCP.A = allGames$PC.V/allGames$PA.V
dat$YPA.H = allGames$PY.H/allGames$PA.H # yards per pass attempt
dat$YPA.V = allGames$PY.V/allGames$PA.V 
dat$YPC.H = allGames$PY.H/allGames$PC.H # yards per pass completion
dat$YPC.V = allGames$PY.V/allGames$PC.V
dat$YRA.H = allGames$RY.H/allGames$RA.H # yards per rush
dat$YRA.V = allGames$RY.V/allGames$RA.V
dat$PY.H = allGames$PY.H # passing yards
dat$PY.V = allGames$PY.V
dat$RY.H = allGames$RY.H # rushing yards
dat$RY.V = allGames$RY.V
dat$SK.H = allGames$SK.V # allGames SK is number of times team WAS SACKED
dat$SK.V = allGames$SK.H
dat$INT.H = allGames$INT.V # allGames INT is number of times team WAS INTERCEPTED
dat$INT.V = allGames$INT.H
dat$FUM.H = allGames$FUM.V # allGames FUM is number of times team fumbled; this is fumble recoveries
dat$FUM.V = allGames$FUM.H
dat$TOP.H = allGames$TOP.H # time possession
dat$TOP.V = allGames$TOP.V
dat$I20.H = allGames$I20.H # pinned opp inside 20
dat$I20.V = allGames$I20.V
dat$RZA.H = allGames$RZA.H # trips to RZ
dat$RZA.V = allGames$RZA.V
dat$RZC.H = allGames$RZC.H # converted RZ
dat$RZC.V = allGames$RZC.V
dat$RZP.H = ifelse(dat$RZA.H == 0, NA, dat$RZC.H/dat$RZA.H) # RZ conversion pct
dat$RZP.V = ifelse(dat$RZA.V == 0, NA, dat$RZC.V/dat$RZA.V) # RZ conversion pctdat$FD.H = allGames$PFD.H + allGames$RFD.H # non-penalty first downs
dat$FD.V = allGames$PFD.V + allGames$RFD.V
# imputing RZ for 2000 with dataset averages
s2000 = which(dat$SEAS == 2000)
sTest = which(is.element(dat$SEAS, c(2009, 2010)))
dat$RZA.H[s2000] = mean(dat$RZA.H[-c(s2000, sTest)])
dat$RZA.V[s2000] = mean(dat$RZA.V[-c(s2000, sTest)])
dat$RZP.H[s2000] = mean(na.exclude(dat$RZP.H[-c(s2000, sTest)]))
dat$RZP.V[s2000] = mean(na.exclude(dat$RZP.V[-c(s2000, sTest)]))
### Choose outcome variable

y = hVsLineMargin
# q = quantile(y, probs = c(.2, .4, .5, .6, .8))
# o = ifelse(y <= q[1], 
#            1, ifelse(y > q[1] & y <= q[2],
#                      2, ifelse(y > q[2] & y <= q[3],
#                                3, ifelse(y > q[3] & y <= q[4],
#                                          4, ifelse(y > q[4] & y <= q[5], 
#                                                    5, ifelse(y > q[5],
#                                                              6, NA))))))
o = ifelse(y > 0, 1, ifelse(y == 0, NA, 0)) # 1 for home team cover spread, NA for push
response = as.factor(o)

to.predict.weeks = 5:16
to.predict.index = is.element(dat$WEEK, to.predict.weeks)
to.predict.dat = dat[to.predict.index, ]

MM = make.MM(response, data = to.predict.dat)

k.weeks = c(1)
value = c(   'WIN',  'WWTS', 'LWTS', 'MARGVS', 'PTS',   'PTS',  'PCP',  'PCP',   'YPC',   'PY',   'PY',  'YRA',  'YRA',   'RY',  'RY',   'SK',   'SK',   'INT',  'INT',  'FUM',  'FUM',  'TOP',  'I20',  'I20',  'RZA',  'RZP',  'RZP',   'FD',   'FD')                              
col.func = c('mean', 'sum',  'sum',   'mean',  'mean',  'mean', 'mean', 'mean',  'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean', 'mean',  'mean', 'mean')
against = c(   F,      F,      F,       F,       F,       T,      F,      T,      F,       F,       T,     F,      T,      F,     T,       F,      T,      F,      T,     F,      T,       F,      F,      T,      F,      F,      T,       F,      T)
diff =    c(   T,      T,      T,       F,       T,       T,      F,      F,      F,       T,       T,     F,      F,      T,     T,       F,      F,      F,      F,     F,      F,       F,      F,      F,      F,      F,      F,       F,      F)

if (new){
  regressors = data.frame(SPR.V = to.predict.dat$SPR.V)
} else{
  regressors = data.frame(rep(NA, nrow(dat)))
}


for (j in k.weeks){
  print(paste('beginning', j, 'week'))
  for (i in 1:length(value)){
    currNames = names(regressors)
    if (diff[i]){
      H.and.V = make.regressors(k = j, against = against[i], all.dat = dat, model.dat = to.predict.dat, col.func[i], value[i])
      H.and.diff = cbind(H.and.V[,1], H.and.V[,1] - H.and.V[,2])
      regressors = cbind(regressors, H.and.diff)
      names(regressors) = c(currNames, names(H.and.V)[1], paste('d.', names(H.and.V)[1], sep = ''))
    } else{
      regressors = cbind(regressors, make.regressors(k = j, against = against[i], all.dat = dat, model.dat = to.predict.dat, col.func[i], value[i]))
    }
    print(paste('finishing', j, 'week', col.func[i], value[i], ifelse(against[i], 'against', 'for')))
  }
}
# Fixing mistakes that should not occur in future runs due to fixed functions/data:
# names(regressors)[1+2*(1:32)] = gsub('(all.)wk\\.(.+)\\.(.+)\\.(.+)\\.(.)', '\\1\\3.\\2.\\4.\\5', names(regressors)[1+2*(1:32)])
# temp = make.regressors(k = 0, against = F, all.dat = dat, model.dat = to.predict.dat, 'mean', 'RZP')
# regressors$all.mean.RZP.f.H = temp[,1]
# regressors$all.mean.RZP.f.V = temp[,2]
# temp = make.regressors(k = 0, against = T, all.dat = dat, model.dat = to.predict.dat, 'mean', 'RZP')
# regressors$all.mean.RZP.a.H = temp[,1]
# regressors$all.mean.RZP.a.V = temp[,2]

if (new){
  MM = cbind(MM, regressors)
}

# can also include differences between columns of regressors
# WIN%.H - WIN%.V; difference in team winning percentages
# (meanPTS.H - meanPTS.V)/meanPTS.H; home outscores visitor percent
# WWTS.H - WWTS.V

### Define training vs test sets
train.seasons = 2000:2008
train.weeks = to.predict.weeks
train.index = is.element(to.predict.dat$SEAS, train.seasons) & is.element(to.predict.dat$WEEK, train.weeks)
test.seasons = 2009:2010
test.weeks = to.predict.weeks
test.index = is.element(to.predict.dat$SEAS, test.seasons) & is.element(to.predict.dat$WEEK, test.weeks)

if (new){
  all.dat = MM
  train.dat = MM[train.index, ]
  test.dat = MM[test.index, ]
  
  save(all.dat, file = 'all.data.Nov1.Rout')
  save(train.dat, file = 'training.data.Nov1.Rout')
  save(test.dat, file = 'test.data.Nov1.Rout')
} else{
  all.dat = load('all.data.Nov1.Rout')
  more.all.dat = data.frame(all.dat, regressors)
  more.train.dat = more.all.dat[train.index, ]
  more.test.dat = more.all.dat[test.index, ]
  
  save(all.dat, file = 'all.data.Nov4.Rout')
  save(train.dat, file = 'training.data.Nov1.Rout')
  save(test.dat, file = 'test.data.Nov1.Rout')
}