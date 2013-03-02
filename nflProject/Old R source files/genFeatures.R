source('cleanData.R')

gameResult = ifelse(dat$Score > dat$OppScore, 1, 
                    ifelse(dat$Score == dat$OppScore, .5, 0))
dat$GameResult = gameResult
rm(gameResult)

Margin = dat$Score - dat$OppScore
dat$Margin = Margin
rm(Margin)

# SqrtMargin = sqrt(abs(dat$Margin))*sign(dat$Margin)
# dat$SqrtMargin = SqrtMargin
# rm(SqrtMargin)

# oppDat = dat[, grep('^Opp', colnames(dat))]
dat = dat[, -grep('^Opp', colnames(dat))]