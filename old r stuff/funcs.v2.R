extract.features = function(df){
  away.index = grep('.+\\.V$', names(df))
  home.index = grep('.+\\.H$', names(df))
  nfeat = (ncol(df)-4)/2 + 4 + 2 # -4 because four col in df are not .v or .h; +4 +2 because I still include them and add 2 more
  nweeks = 21
  newdat = list()
  s = unique(df$SEAS)
  for (season in 1:length(s)){
    this.s = s[season]
    this.s.index = which(df$SEAS == this.s)
    newdat[[paste(this.s)]] = list()
    t = unique(df[this.s.index, 'V'])
    for (team in 1:length(t)){
      this.t = t[team]
      newdat[[paste(this.s)]][[paste(this.t)]] = matrix(rep(NA, nweeks*nfeat), nrow = nweeks)
      row.names(newdat[[paste(this.s)]][[paste(this.t)]]) = 1:21
      colnames(newdat[[paste(this.s)]][[paste(this.t)]]) = c('H/A', 'OPP', gsub('(.+)\\..+', '\\1', names(df))[c(1:4, away.index)])
    }
  }
  for (i in 1:nrow(df)){
    away.team = df[i, 'V']
    home.team = df[i, 'H']
    row.s = df[i, 'SEAS']
    row.wk = df[i, 'WEEK']
    away.info = c('A', as.character(df[i, 'H']))
    home.info = c('H', as.character(df[i, 'V']))
    away.dat = as.numeric(df[i, c(1:4, away.index)])
    home.dat = as.numeric(df[i, c(1:4, home.index)])
    newdat[[paste(row.s)]][[paste(away.team)]][row.wk, ] = c(away.info, away.dat)
    newdat[[paste(row.s)]][[paste(home.team)]][row.wk, ] = c(home.info, home.dat)
  }
  newdf.list = list()
  for (season in 1:length(newdat)){
    newdf.list[[paste(names(newdat)[season])]] = list()
    for (team in 1:length(newdat[[season]])){
      newdf.list[[season]][[paste(names(newdat[[season]])[team])]] = as.data.frame(newdat[[season]][[team]])
      for (feature in 1:ncol(newdat[[season]][[team]])){
        if (feature<7){
          newdf.list[[season]][[team]][,feature] = newdat[[season]][[team]][,feature]
        } else {
          newdf.list[[season]][[team]][,feature] = as.numeric(newdat[[season]][[team]][,feature])
        }
      }
    }
  }
  return(newdf.list)
}

k.means = function(list, index, k = 1:5,  w = 17){
  orig.names = names(list[[1]][[1]])[index]
  column.names = c()
  for (lazy in 1:length(index)){
    column.names = c(column.names, paste(orig.names[lazy], '.', k, 'wk', sep = ''))
  }
  new.list = list()
  for (season in 1:length(list)){
    this.s = names(list)[season]
    new.list[[paste(this.s)]] = list()
    for(team in 1:length(list[[season]])){
      this.t = names(list[[season]])[team]
      df = list[[season]][[team]]
      newdat = matrix(rep(NA, w*length(index)*length(k)), nrow = w)
      colnames(newdat) = column.names
      for (week in 1:17){ #looping over weeks
        
        new.row = c() # make a new row for each week in each df
        for (feature in 1:length(index)){
          
          #      column.names = c(column.names, paste(names(df)[index[feature]], k))
          temp = c()
          for(num.wks in 1:length(k)){ #looping over k weeks average
            # need to fix to exclude bye week from k weeks average
            span = k[num.wks]
            if (week - span > 0){
              to.average = (week - span):(week-1)
              # print(to.average); print(index[feature])
              #print(na.exclude(as.numeric(df[to.average, index[feature]])))
              temp[num.wks] = ifelse(week - span >= 1,
                                     mean(na.exclude(as.numeric(df[to.average, index[feature]]))),
                                     'else')
            } else{
              temp[num.wks] = NA          
            }
          }
          new.row = c(new.row, temp)
        }
        newdat[week, ] = new.row
      }
      new.list[[paste(this.s)]][[paste(this.t)]] = as.data.frame(newdat)
    }
  }
  return(new.list)
}
