setwd('~/Projects/nfl stuffs')
gameindex = read.csv("~/Projects/nfl stuffs/gameindex.csv", T, ",")
View(gameindex)
gamelines = gameindex$SPR.V
slope = coef(lm(game.lines~x))[2]
gameprobs.1 = gamelines/slope
gameprobs.2 = gameprobs.1+50
gameprobs.3 = gameprobs.2*.01

names(gameindex)

library('gbm')
home.win = ifelse(gameindex[,256]>=0, 1, 0)
home.vs.spread = ifelse( (gameindex$WinMar.H-gameindex$SPR.V)>=0, 1, 0)

gf = gbm.fit(gameindex[,-c(1:17, 256)], home.vs.spread,
             train = .7, shr = .1, n.tree = 250, dist = 'bernoulli')
tree = gbm.perf(gf, meth = 'test'); tree

