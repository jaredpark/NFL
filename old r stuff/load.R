gameindex = read.csv("gameindex.csv", T, ",")
source('funcs.v2.R')

game.info = gameindex[ , 2:13]
game.lines = gameindex[ , 14:17]
first.downs = gameindex[ , 18:23]
rushing = gameindex[ , 24:27]
passing = gameindex[ , 28:33]
sacks = gameindex[ , 34:35]
turnovers = gameindex[ , 36:39]
game.control = gameindex[ , c(58,59,74:77)] # TOP and RZ 

to.expand = cbind(game.info[, c(1, 2, 4, 5)], first.downs, rushing, passing, sacks, turnovers, game.control)

raw.list = extract.features(to.expand)

a = k.means(raw.list, index = 10:13, k = 1:2)
# not working

#####
# for stat 153 project:
#####

# want df with home and away team dummies to begin
library('dummies')
# grab the relevant data from gameindex
dat = gameindex[, c('GAME.ID', 'SEAS', 'WEEK', 'V', 'H', 'PTS.V', 'PTS.H')]
# create matrix of dummy variables for home and away teams
home.mm = dummy(dat$H, sep = 'omeIs')
away.mm = dummy(dat$V, sep = 'isIs')
# use the sign of the difference b/w home and away scores for class
score.diff = dat$PTS.H - dat$PTS.V
which(score.diff == 0)
# there are two tie games in the dataset, ~0.07% of dataset. Fine to just choose win or loss for these
class = ifelse(score.diff >= 0, 1, 0)
simpledat = data.frame(cbind(class, home.mm, away.mm))
simpleFit = glm(class~., dat = simpledat, family = binomial(link = logit))
# the fitted values are probabilities, while logit(fitted) gives the
# sum of the fit intercept and the two dummy coef for the game
library(boot)
predictions = ifelse(simpleFit$fitted >= .5, 1, 0)
mean(predictions == class)
sum(simpleFit$fitted)/sum(class)
hist(simpleFit$fitted, breaks = 20)
plot(1:nrow(dat), score.diff)