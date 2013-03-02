WinMargin.H=gameindex$PTS.H-gameindex$PTS.V #making a vector with home winning margin
gameindex[,256]=rep(NA,length(gameindex[,1])) #adding a column for above data
gameindex=data.frame(gameindex, "WinMar.H" = WinMargin.H) #adding data to dataframe
View(gameindex)
mn.games=gameindex[gameindex$DAY=="MN", ] #new dataframe with only MN games
View(mn.games)

hist(gameindex$SPR.V-gameindex$WinMar.H, breaks=50)
par(mfrow=c(1,1))
hist(gameindex$SPR.V, breaks = 50)
hist(gameindex$WinMar.H, breaks = -50:60) #just looking at the data

#now a loop to go through the dataframe and identify all teams
#that played on MN in the previous week, making a new dataframe
#with ALL of the data for all games in the week after MNF appearance

#going to make a list with 11 drawers for each of the 11 seasons
gamelist = list()
for(i in 1:length(unique(gameindex$SEAS))){
  gamelist[[i]] = data.frame(gameindex[gameindex$SEAS==unique(gameindex$SEAS)[i],-2])
}
#now checking the number of games within each list
length(gamelist[[3]][,1])
length(gameindex[gameindex$SEAS==2006, 1])

#now splitting gamelist in to 11 drawers each with 21 drawers for the weeks of each season
newlist = list()
for (j in 1:length(gamelist)){
  newlist[[j]]=list()
  for (i in 1:max(gamelist[[j]]$WEEK)){ #could have used length(unique()) instead of max()
    data = gamelist[[j]][gamelist[[j]]$WEEK==unique(gamelist[[j]]$WEEK)[i], -2]
#        print(paste(i, j))
#    newlist[[j]][i]=list()
    newlist[[j]][[i]] = data.frame(data)
  }
}

#now making an index for the game ids for the MN games

for (j in 1:length(newlist)){
  for (i in 1:max(newlist[[j]][[i]]$WEEK)){