library(dplyr)
library(GenSA)




del_cols = c('DATE_ACCOUNT_OPENED',
             'DATE_LAST_PAYMENT')

gini_index2 = function(cutoff, classes, var ) {
  if(is.numeric(var) | class(var) == 'Date'){
    splitvar = var < cutoff  
  } else if(is.character(var) | is.factor(var)){
    splitvar = var %in% cutoff
  } else {
    stop('var is not numeric/Date/char or factor')
  }
  if(length(table(splitvar)) < 2){
    return(0)
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Node_Gini <- sum(crossprob[,1]**2)
  Yes_Node_Gini <- sum(crossprob[,2]**2)
  # weight gini by base_prob:
  return(-sum(base_prob * c(No_Node_Gini,Yes_Node_Gini)))
}


all_gini = function(df, target_col_name, features_cols) {
  num.gini = data.frame()
  cat.gini = data.frame()
  cols = c('AMOUNT_PAST_DUE',
           'Loan_amount1',
           'Loan_amount2',
           'Loan_amount3',
           'Loan_amount4',
           'Loan_amount5',
           'Loan_amount6',
           'Amount_owe1',
           'Amount_owe2',
           'Amount_owe3',
           'Amount_owe4',
           'Amount_owe5',
           'Amount_owe6',
           'Amount_Owe_Momentum1',
           'Amount_Owe_Momentum2',
           'Amount_Owe_Momentum3',
           'Amount_Owe_Momentum4',
           'Amount_Owe_Momentum5',
           'stdUtil123',
           'stdUtil456',
           'stdUtil1to6')
  for(i in features_cols){
    pt = proc.time()
    
    if(names(df)[i] %in% cols){
      
    }
    
    if(is.numeric(df[[i]]) | class(df[[i]]) == 'Date'){
      print(paste0('number of NA in ', names(df)[i], ' = ', sum(is.na(df[i]))))
      
      
      all.threshold = unique(df[[i]])
      score = sapply(all.threshold, function(x) { gini_index(df[[target_col_name]], df[[i]] < x ) })
      tmp = data.frame(feature.name = rep(names(df)[i], length(all.threshold)),
                       threshold.num = all.threshold,
                       threshold.date = as.Date(NA),
                       gini.score = score,
                       stringsAsFactors = F)
      num.gini = rbind(num.gini, tmp)
    } else if(class(df[[i]]) == 'Date') {
      print(paste0('number of NA in ', names(df)[i], ' = ', sum(is.na(df[i]))))
      all.threshold = unique(df[[i]])
      score = sapply(all.threshold, function(x) { gini_index(df[[target_col_name]], as.numeric(df[[i]]) < as.numeric(x) ) })
      tmp = data.frame(feature.name = rep(names(df)[i], length(all.threshold)),
                       threshold.date = all.threshold,
                       threshold.num = as.numeric(NA),
                       gini.score = score,
                       stringsAsFactors = F)
      num.gini = rbind(num.gini, tmp)
    } else {
      stop(paste0(names(df)[i],' is not numeric or Date'))
    }
    print(proc.time() - pt)
  }
  return(num.gini)
}

# for categorical independent var:
all_gini_cat = function(df, target_col_name, features_col_range){
  cat.gini = data.frame(feature.name = character(),
                        group1 = character(),
                        group2 = character(),
                        gini.score = numeric(),
                        stringsAsFactors = F)
  for(i in features_col_range){
    pt = proc.time()
    if(is.character(df[[i]]) | is.factor(df[[i]])){
      print(paste0('number of NA in ', names(df)[i], ' = ', sum(is.na(df[i]))))
      values = unique(df[[i]])
      for(group.num in 1:(length(values)/2)){
        group1.num = group.num
        group2.num = length(values) - group1.num
        group1.prob = as.data.frame(combn(values, group1.num))
        for(group1.case in 1:ncol(group1.prob)){
          gini.case = gini_index(df[[target_col_name]], df[[i]] %in% group1.prob[[group1.case]])
          tmp = data.frame(feature.name = names(df)[i],
                           group1 = paste(group1.prob[[group1.case]], collapse = ', '),
                           group2 = paste(values[!(values %in% group1.prob[[group1.case]])], collapse = ', '),
                           gini.score = gini.case,
                           stringsAsFactors = F)
          cat.gini = rbind(cat.gini, tmp)
        }
      }
    } else {
      stop(paste0(names(df)[i],' is not character or factor'))
    }
    print(proc.time() - pt)
  }
  return(cat.gini)
}






# find column number of numeric/date and categorical independent vars: 
col.num = c()
col.cat = c()
for(i in 1:ncol(train_cc)){
  if(is.numeric(train_cc[[i]]) | class(train_cc[[i]]) == 'Date'){
    col.num = append(col.num, i)
  } else if(is.character(train_cc[[i]]) | is.factor(train_cc[[i]])) {
    col.cat = append(col.cat, i)
  }
}
col.cat = col.cat[-length(col.cat)] 

# check if number of columns is correct:
ncol(train_cc)
length(col.cat) + length(col.num)

# call gini function for numeric/date and categorical:
gini.num = all_gini_num(train_cc, 'Ever30plus_n12MTH', c(29,47))




##### TESTING :
optimize(f = gini_index2,
         classes = train_cc$Ever30plus_n12MTH,
         var = as.numeric(train_cc$DATE_ACCOUNT_OPENED) ,
         interval = c(15401, 16708))

x = unique(train_cc$Loan_amount1)
y = c()
for(i in 1:length(x)){
  y = append(y, gini_index2(classes = train_cc$Ever30plus_n12MTH,
              var = train_cc$Loan_amount1,
              cutoff = x[i]))
  
}

min(sapply(x, FUN =gini_index2, classes = train_cc$Ever30plus_n12MTH, var = train_cc$Loan_amount1))






cutoff = seq(min(train_cc$Loan_amount1), max(train_cc$Loan_amount1),
             length.out = 10) 

cutoffs = boxplot.stats(as.numeric(unique(train_cc$stdUtil1to6)))$stats
len = seq(cutoffs[1], cutoffs[5], length.out = 10)
globalmin = 0
for(i in 1:(length(len)-1)){
  sa = GenSA(fn = gini_index2, 
             classes = train_cc$Ever30plus_n12MTH, 
             var = as.numeric(train_cc$DATE_ACCOUNT_OPENED), 
             lower = len[i], 
             upper = len[i+1],
             control = list(max.time = 10))
  if(sa$value < globalmin){
    globalSA = sa
  }
}









simulated_annealing <- function(func,s0, niter = 1000, step = 0.01, ...) {
  
  # Initialize
  ## s stands for state
  ## f stands for function value
  ## b stands for best
  ## c stands for current
  ## n stands for neighbor
  states = c()
  s_n <- s0
  f_n <- func(splitvar = train_cc$Loan_amount1 < s_n, ...)
  while(f_n == 0){
    s_n <- abs(rnorm(1, s_n, 2000))
    f_n <- func(splitvar = train_cc$Loan_amount1 < s_n, ...)
  }
  s_b <- s_c <- s_n
  f_b <- f_c <- f_n
  message("It\tBest\t\tCurrent\t\tNeigh\t\tTemp\t\tS_Current")
  message(sprintf("%i\t%.7f\t%.7f\t%.7f\t%.7f\t%.7f", 0L, f_b, f_c, f_n, 1, s_c))
  
  for (k in 1:niter) {     
    Temp <- (1 - step)^k
    # consider a random neighbor
    repeat{
      s_n <- rnorm(1, s_c, 2000)
      f_n <- func(splitvar = train_cc$Loan_amount1 < s_n, ...)
      if(s_n > s0 && f_n > 0){
        break
      }
    }
    
    # update current state
    if (f_n > f_c || runif(1, 0, 1) < exp(-(f_n - f_c) / Temp)) {
      s_c <- s_n
      f_c <- f_n
    }
    # update best state
    if (f_n > f_b) {
      s_b <- s_n
      f_b <- f_n         
    }
    #message(sprintf("%i\t%.7f\t%.7f\t%.7f\t%.7f\t%.7f", k, f_b, f_c, f_n, Temp, s_c))
    states = append(states, s_c)
    if(Temp < 0.005){
      break
    }
  }
  return(list(iterations = niter, best_value = f_b, best_state = s_b, all_state = states))
}

sol <- simulated_annealing(gini_index, classes = train_cc$Ever30plus_n12MTH, s0 = min(train_cc$Loan_amount1))

