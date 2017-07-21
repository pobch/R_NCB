library(dplyr)
library(GenSA)


gini_index2 = function(cutoff, classes, var ) {
  splitvar = var < cutoff
  if (is.null(splitvar)) {
    base_prob = table(classes)/length(classes)
    return(sum(base_prob**2))
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
         var = train_cc$Loan_amount1,
         interval = c(min(train_cc$Loan_amount1), max(train_cc$Loan_amount1)))

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
sa = GenSA(par = cutoff[1],
           fn = gini_index2, 
           classes = train_cc$Ever30plus_n12MTH, 
           var = train_cc$Loan_amount1, 
           lower = min(train_cc$Loan_amount1), 
           upper = max(train_cc$Loan_amount1),
           control = list(max.time = 3))









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

