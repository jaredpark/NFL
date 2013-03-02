odds = readLines("http://www.vegasinsider.com/nfl/odds/las-vegas/")
p.win.underdog = .5*c(0.6082949309, 0.8526077098, 0.5028284098, 
                      0.7037037037, 0.3959627329, 0.3072463768,
                      0.8526077098, 0.5462653289, 0.457439896, 
                      0.8712121212, 0.890648194, 0.5651741294
                      )
p.win.favorite = 1 - p.win.underdog
game.lines = c(6, 3, 7.5, 
               4, 10, 13,
               3, 6.5, 7.5,
               2.5, 2, 7
               )
x=100*(p.win.favorite-.5)
par(lab = c(10,10, 5))
plot(x, game.lines,
     xlab = "Favorite p(win) above 50%",
     xlim = c(0,50),
     ylim = c(0,14))
lm(game.lines~x)
abline(coef(lm(game.lines~x))[1], 
       coef(lm(game.lines~x))[2])
abline(h = seq(0,14, by = .5), col = "grey")
#approximately, for this small data set, with a
#simple linear regression that does not inspect
#for possible 'bin' strategies and ignores the
#importance of 'key numbers' in NFL wagering; for
#each 1% above 50% for the probability of winning
#for the favored team leads to an increase in the
#betting line by 0.32; a 60% p(win) equates to 
#roughly a -3 line for the game

