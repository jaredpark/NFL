# originating file: fresh.funcs.v3.R

# make.MM = function(response, index = 1:nrow(data), data = allGames){
#   ### Creates a model matrix with reponse, home and away team names, the game id, and 
#   ### the week and season of the game.
#   ### Function is accessed through the get.model.stat function
#   
#   MM = data.frame(response = c(), ID = c(), H = c(), V = c(), SEAS = c(), WK = c())
#   MM.dat = data[index,]
#   
#   y = response[index]
#   
#   for (i in 1:nrow(MM.dat)){
#     s = MM.dat$SEAS[i]
#     w = MM.dat$WEEK[i]
#     home.team = as.character(MM.dat$H[i])
#     away.team = as.character(MM.dat$V[i])
#     MM[i, 1] = y[i]
#     MM[i, 2] = MM.dat$GAME.ID[i]
#     MM[i, 3] = home.team
#     MM[i, 4] = away.team
#     MM[i, 5] = s
#     MM[i, 6] = w
#   }
#   names(MM) = c('response', 'ID', 'H', 'V', 'SEAS', 'WEEK')
#   MM$response = factor(as.numeric(MM$response))
#   return(MM)
# }

initialize.MM = function(data = allGames, rowIndex = 1:nrow(data), colNames){
  ### Creates a model matrix with home and away team names, week and season of the game.
  
  MM = data.frame(SEAS = c(), WK = c(), H = c(), V = c())
  MM.dat = data[rowIndex,]
  
  for (i in 1:nrow(MM.dat)){
    s = MM.dat$SEAS[i]
    w = MM.dat$WEEK[i]
    home.team = as.character(MM.dat$H[i])
    away.team = as.character(MM.dat$V[i])
    MM[i, 1] = y[i]
    MM[i, 2] = MM.dat$GAME.ID[i]
    MM[i, 3] = home.team
    MM[i, 4] = away.team
    MM[i, 5] = s
    MM[i, 6] = w
  }
  names(MM) = c('response', 'ID', 'H', 'V', 'SEAS', 'WEEK')
  MM$response = factor(as.numeric(MM$response))
  return(MM)
}

get.team.k.week.data = function(k, team, variable, against, data, use.prev.seas = F){
  team.games = ifelse(data$H == team, 'H', ifelse(data$V == team, 'V', 0))
  last.k.week.dat = c()
  team.index = (data$H == team | data$V == team)
  data = data[team.index, ]
  for (i in 1:nrow(data)){
    if (data$H[i] == team){
      if (against){
        var.name = paste(variable, '.V', sep = '')
        temp = data[i, var.name]
        last.k.week.dat = c(last.k.week.dat, temp)
      } else{
        var.name = paste(variable, '.H', sep = '')
        temp = data[i, var.name]
        last.k.week.dat = c(last.k.week.dat, temp)
      }} else if (data$V[i] == team){
        if (against){
          var.name = paste(variable, '.H', sep = '')
          temp = data[i, var.name]
          last.k.week.dat = c(last.k.week.dat, temp)
        } else{
          var.name = paste(variable, '.V', sep = '')
          temp = data[i, var.name]
          last.k.week.dat = c(last.k.week.dat, temp)
        }}
  }
  if (k == 0){ # k = 0 is interpreted as all prev data for the season
    to.return = 1:length(last.k.week.dat)
  } else{
    if (!use.prev.seas){
      to.return = (length(last.k.week.dat)-k+1):length(last.k.week.dat)
    } else {
      stop('still have work to do for prev season data')
    }
  }
  if (is.element(0, to.return)){
    to.return = 1:length(last.k.week.dat)
  }
  return(na.exclude(last.k.week.dat[to.return]))
}

get.values = function(value, collapse.func, k, team, against, data, use.prev.seas = F){
  last.k.wk.value = get.team.k.week.data(k, team, value, against, data, use.prev.seas)
  if (collapse.func == 'mean'){
    out = mean(last.k.wk.value)
  } else if (collapse.func == 'sum'){
    out = sum(last.k.wk.value)
  } else {stop('only mean and sum supported for collapse.func')}
  return(out)
}

get.all.team.k.wk.func = function(k, week, season, against, data, collapse.func, value, use.prev.seas = F){
  if (k == 0){
    is.prev.weeks = data$SEAS == season & data$WEEK < week
  } else if (use.prev.seas){    
    all.eff.weeks = data$WEEK + (data$SEAS - 2000)*17
    this.eff.week = week + (season - 2000)*17
    is.prev.weeks = all.eff.weeks <= this.eff.week
  } else{
    is.prev.weeks = data$SEAS == season & data$WEEK <= week - 1
  }
  newdat = data[is.prev.weeks, ]
  teams = unique(c(unique(as.character(newdat$V)), unique(as.character(newdat$H))))
  func.values = rep(NA, length(teams))
  names(func.values) = teams
  team.num = 1
  for (team in teams){
    func.values[team.num] = get.values(value, collapse.func, k, team, against, data = newdat, use.prev.seas)
    team.num = team.num + 1
  }
  return(func.values)
}

make.regressors = function(k, against = F, all.dat, model.dat, collapse.func, value = NA, use.prev.seas = F){
  prev.wk = 0
  prev.seas = 0
  home = rep(NA, nrow(model.dat))
  away = rep(NA, nrow(model.dat))
  for (i in 1:nrow(model.dat)){
    wk = model.dat$WEEK[i]
    seas = model.dat$SEAS[i]
    if (prev.wk == wk & prev.seas == seas){
    } else{
      all.team.curr.wk.value = get.all.team.k.wk.func(k, wk, seas, against, all.dat, collapse.func, value, use.prev.seas)
    }
    home.team = as.character(model.dat$H[i])
    away.team = as.character(model.dat$V[i])
    home[i] = all.team.curr.wk.value[home.team]
    away[i] = all.team.curr.wk.value[away.team]
    prev.wk = wk
    prev.seas = seas
  }
  if (k == 0){ k = 'all'}
  for.or.against = ifelse(against, 'a', 'f')
  nameH = paste(k, '.', collapse.func, '.', value, '.', for.or.against, '.H', sep = '')
  nameV = paste(k, '.', collapse.func, '.', value, '.', for.or.against, '.V', sep = '')
  out = data.frame(home, away)
  names(out) = c(nameH, nameV)
  return(out)
}