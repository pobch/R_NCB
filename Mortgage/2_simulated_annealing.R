library(dplyr)
library(GenSA)
library(combinat)

#input at splitVar is T/F
gini_index = function(classes,splitvar = NULL) {
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
  return(sum(base_prob * c(No_Node_Gini,Yes_Node_Gini)))
}

#Input splitvar & Cutoff
gini_index2 = function(cutoff, classes, var ) {
  if(is.numeric(var)){
    splitvar = var < cutoff  
  } else if(is.character(var) | is.factor(var)){
    splitvar = var %in% cutoff
  } else {
    stop('var is not numeric/char or factor')
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

# t = sapply(unique(train_cc$Amount_Owe_Momentum1), gini_index2, classes = train_cc$Ever30plus_n12MTH, var = train_cc$Amount_Owe_Momentum1)


# target_col , predictor are vector type
simA2 = function(target_col, predictor) {
  if(min(as.numeric(predictor)) > 0){
    low = min(as.numeric(predictor))
  } else{
    low = min(0 , quantile(as.numeric(predictor), 0.01))
  }
  high = quantile(as.numeric(predictor), 0.99)
  sa = GenSA(fn = gini_index2, 
             classes = target_col, 
             var = as.numeric(predictor), 
             lower = low, 
             upper = high,
             control = list(max.time = 4))
  return(sa)
}
#for numeric predictor with GenSA
num.gini = function(df, target_num, predictor_num){
  print(paste0('processing ', names(df)[predictor_num], ', NA = ', sum(is.na(df[predictor_num]))))
  sa.score = simA2(df[[target_num]], df[[predictor_num]])
  result = data.frame(feature.name = names(df)[predictor_num],
                   threshold.num = sa.score$par,
                   gini = sa.score$value,
                   stringsAsFactors = F)
  return(result)
}

#for numeric predictor without GenSA
num.gini.delcols = function(df, target_num, predictor_num){
  print(paste0('processing ', names(df)[predictor_num], ', NA = ', sum(is.na(df[predictor_num]))))
 
  #tmp show Name/Cutoff/Gini score
  tmp = data.frame(feature.name = names(df)[predictor_num],
                   threshold.num = unique(df[[predictor_num]]),
                   gini = sapply(unique(df[[predictor_num]]), gini_index2, classes = df[[target_num]], var = df[[predictor_num]]),
                   stringsAsFactors = F)
  result = tmp[which.min(tmp$gini),]
  return(result)
}

#for categorical predictor
cat.gini = function(df, target_num, predictor_num){
  print(paste0('processing ', names(df)[predictor_num], ', NA = ', sum(is.na(df[predictor_num]))))
  result = data.frame()
  values = unique(df[[predictor_num]])
  for(group.num in 1:(length(values)/2)){
    group1.num = group.num
    group2.num = length(values) - group1.num
    group1.prob = as.data.frame(combn(values, group1.num))
      for(group1.case in 1:ncol(group1.prob)){
        gini.case = gini_index(df[[target_num]], df[[predictor_num]] %in% group1.prob[[group1.case]])
        tmp = data.frame(feature.name = names(df)[predictor_num],
                         threshold.cat.g1 = paste(group1.prob[[group1.case]], collapse = ', '),
                         threshold.cat.g2 = paste(values[!(values %in% group1.prob[[group1.case]])], collapse = ', '),
                         gini = -gini.case,
                         stringsAsFactors = F)
        result = rbind(result, tmp)
      }
  }
  result = result %>% 
    mutate(the.rank = rank(gini, ties.method = 'random')) %>% 
    filter(the.rank == 1) %>% 
    select(-the.rank)
  return(result)
}


all_gini = function(df, depen_var_num, predictor_range, except_col){
  result = data.frame()
  for(i in predictor_range){
    if(is.numeric(df[[i]]) & !(names(df)[i] %in% except_col)){
      result = bind_rows(result, num.gini(df, depen_var_num, i))
    } else if (is.numeric(df[[i]]) & names(df)[i] %in% except_col) {
      result = bind_rows(result, num.gini.delcols(df, depen_var_num, i))
    } else if (is.character(df[[i]])) {
      result = bind_rows(result, cat.gini(df, depen_var_num, i))
    } else {
      print(paste0('skip ', names(df)[i]))
    }
  }
  return(result)
}


thres.changer = function(gini.table, df, depen_var_col, predictor_col, new.thres){
  result = data.frame()
  if(is.numeric(df[[predictor_col]])){
    tmp = data.frame(feature.name = predictor_col,
                     threshold.num = new.thres,
                     gini = gini_index2(new.thres, df[[depen_var_col]], df[[predictor_col]]),
                     stringsAsFactors = F)
  } else if (is.character(df[[predictor_col]])) {
    values = unique(df[[predictor_col]])
    gini.case = gini_index(df[[depen_var_col]], df[[predictor_col]] %in% new.thres)
    tmp = data.frame(feature.name = predictor_col,
                     threshold.cat.g1 = paste(new.thres, collapse = ', '),
                     threshold.cat.g2 = paste(values[!(values %in% new.thres)], collapse = ', '),
                     gini = -gini.case,
                     stringsAsFactors = F)
  } else {
    print(paste0('skip ', names(df)[i]))
  }
  result = bind_rows(gini.table, tmp)
  return(result)
}


del_cols2 = c('Momentum1',
              'Momentum2',
              'Momentum3',
              'Momentum4',
              'Momentum5',
              'Min_Momentum',
              'Max_Momentum',
              'Number_of_1',
              'Number_of_1plus',
              'Number_of_2',
              'Number_of_2plus',
              'Number_of_3plus',
              'Num_CC',
              'Num_A_CC',
              'nUtil123',
              'nUtil456',
              'nUtil1to6',
              'Delay_Payment_code1', 
              'Delay_Payment_code2', 
              'Delay_Payment_code3', 
              'Delay_Payment_code4', 
              'Delay_Payment_code5', 
              'Delay_Payment_code6')
all.gini = all_gini(train_cc, 115, 1:114, del_cols2)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)


test = thres.changer(all.gini, train_cc, 'Ever30plus_n12MTH', 'Utilization1', 0.7)
test %>% arrange(gini)

# 
# gini.num2 = read.csv('D:\\gini_num.csv', stringsAsFactors = FALSE)
# gini.num2 = gini.num2 %>% 
#   group_by(feature.name) %>% 
#   mutate(the.rank = rank(-gini.score, ties.method = 'random')) %>% 
#   filter(the.rank == 1) %>% 
#   select(-the.rank) %>% 
#   ungroup() %>% 
#   mutate(the.rank = rank(-gini.score)) %>% 
#   arrange(desc(gini.score))
# 
# compare = gini.num2 %>% 
#   left_join(all.gini, by = 'feature.name') %>% 
#   mutate(percent = round(abs(threshold.num.x - threshold.num.y) / threshold.num.x * 100,2))
# compare$percent = round(abs(compare$threshold.num.x - compare$threshold.num.y) / compare$threshold.num.x * 100,2)
# 


##################### END ####################################

# 
# 
# 
# 
#   
#   
#   
# all_gini = function(df, target_col_name, features_cols) {
#   num.gini = data.frame()
#   cat.gini = data.frame()
#   date.gini = data.frame()
#   # cols = c('AMOUNT_PAST_DUE',
#   #          'Loan_amount1',
#   #          'Loan_amount2',
#   #          'Loan_amount3',
#   #          'Loan_amount4',
#   #          'Loan_amount5',
#   #          'Loan_amount6',
#   #          'Amount_owe1',
#   #          'Amount_owe2',
#   #          'Amount_owe3',
#   #          'Amount_owe4',
#   #          'Amount_owe5',
#   #          'Amount_owe6',
#   #          'Amount_Owe_Momentum1',
#   #          'Amount_Owe_Momentum2',
#   #          'Amount_Owe_Momentum3',
#   #          'Amount_Owe_Momentum4',
#   #          'Amount_Owe_Momentum5',
#   #          'stdUtil123',
#   #          'stdUtil456',
#   #          'stdUtil1to6')
#   for(i in features_cols){
#     pt = proc.time()
#     
#     # if(names(df)[i] %in% cols){
#     #   
#     # }
#     
#     if(is.numeric(df[[i]]) & !(names(df)[i] %in% del_cols2)){
#       print(paste0('number of NA in ', names(df)[i], ' = ', sum(is.na(df[i]))))
#       
#       
#       all.threshold = unique(df[[i]])
#       score = sapply(all.threshold, function(x) { gini_index(df[[target_col_name]], as.numeric(df[[i]]) < as.numeric(x) ) })
#       sa.score = simA(df[[target_col_name]], df[[i]])
#       tmp = data.frame(feature.name = names(df)[i],
#                        threshold.num = all.threshold[which.max(score)],
#                        threshold.date = all.threshold[which.max(score)],
#                        gini.score = max(score, na.rm = TRUE),
#                        sa.threshold.num = sa.score$par,
#                        sa.gini = sa.score$value,
#                        stringsAsFactors = F)
#       num.gini = rbind(num.gini, tmp)
#     } else if(is.character(df[[i]]) | is.factor(df[[i]])){
#       print(paste0('number of NA in ', names(df)[i], ' = ', sum(is.na(df[i]))))
#       values = unique(df[[i]])
#       for(group.num in 1:(length(values)/2)){
#         group1.num = group.num
#         group2.num = length(values) - group1.num
#         group1.prob = as.data.frame(combn(values, group1.num))
#         for(group1.case in 1:ncol(group1.prob)){
#           gini.case = gini_index(df[[target_col_name]], df[[i]] %in% group1.prob[[group1.case]])
#           tmp = data.frame(feature.name = names(df)[i],
#                            group1 = paste(group1.prob[[group1.case]], collapse = ', '),
#                            group2 = paste(values[!(values %in% group1.prob[[group1.case]])], collapse = ', '),
#                            gini.score = gini.case,
#                            stringsAsFactors = F)
#           cat.gini = rbind(cat.gini, tmp)
#         }
#       }
#       
#     } else if(names(df)[i] %in% del_cols2 | class(df[[i]]) == 'Date'){
#       next
#     } else {
#       stop(paste0(names(df)[i],' is not numeric/Date/char/factor'))
#     }
#     print(proc.time() - pt)
#   }
#   if(dim(cat.gini)[1] != 0){
#     cat.gini = cat.gini %>% 
#       group_by(feature.name) %>% 
#       mutate(the.rank = rank(-gini.score, ties.method = 'random')) %>% 
#       filter(the.rank == 1) %>% 
#       select(-the.rank)
#   }
#   
#   conclude = num.gini %>% 
#     bind_rows(cat.gini) %>% 
#     arrange(desc(gini.score))
#   return(conclude)
# }
# 
# pob = all_gini(train_cc, 'Ever30plus_n12MTH', 21:40)
# pob2 = all_gini(train_cc, 'Ever30plus_n12MTH', 41:50)
# pob3 = all_gini(train_cc, 'Ever30plus_n12MTH', 51:55)
# pob4 = all_gini(train_cc, 'Ever30plus_n12MTH', 56:76)
# pob5 = all_gini(train_cc, 'Ever30plus_n12MTH', 77:114)
# 
# 
# 
# 
# 
# #------------------------
# 
# 
# 
# 
# pob.gini = gini.sim(train_cc)
# 
# gini.num2 = read.csv('D:\\gini_num.csv', stringsAsFactors = FALSE)
# 
# compare = gini.num2 %>% 
#   left_join(pob.gini, by = 'feature.name') %>% 
#   group_by(feature.name) %>% 
#   mutate(the_rank = rank(-gini.score, ties.method = 'random')) %>% 
#   ungroup() %>% 
#   filter(the_rank == 1) %>% 
#   select(-the_rank)
# compare$sa.gini = -compare$sa.gini
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###################################################################### TESTING :
# optimize(f = gini_index2,
#          classes = train_cc$Ever30plus_n12MTH,
#          var = as.numeric(train_cc$DATE_ACCOUNT_OPENED) ,
#          interval = c(15401, 16708))
# 
# x = unique(train_cc$Loan_amount1)
# y = c()
# for(i in 1:length(x)){
#   y = append(y, gini_index2(classes = train_cc$Ever30plus_n12MTH,
#               var = train_cc$Loan_amount1,
#               cutoff = x[i]))
#   
# }
# 
# min(sapply(x, FUN =gini_index2, classes = train_cc$Ever30plus_n12MTH, var = train_cc$Loan_amount1))
# 
# 
# 
# 
# 
# 
# cutoff = seq(min(train_cc$Loan_amount1), max(train_cc$Loan_amount1),
#              length.out = 10) 
# 
# cutoffs = boxplot.stats(as.numeric(unique(train_cc$stdUtil1to6)))$stats
# len = seq(cutoffs[1], cutoffs[5], length.out = 10)
# globalmin = 0
# for(i in 1:(length(len)-1)){
#   sa = GenSA(fn = gini_index2, 
#              classes = train_cc$Ever30plus_n12MTH, 
#              var = as.numeric(train_cc$DATE_ACCOUNT_OPENED), 
#              lower = len[i], 
#              upper = len[i+1],
#              control = list(max.time = 10))
#   if(sa$value < globalmin){
#     globalSA = sa
#   }
# }
# 
# 
# 
# 
# 
# ################## TEST 2 :
# 
# 
# 
# simulated_annealing <- function(func,s0, niter = 1000, step = 0.01, ...) {
#   
#   # Initialize
#   ## s stands for state
#   ## f stands for function value
#   ## b stands for best
#   ## c stands for current
#   ## n stands for neighbor
#   states = c()
#   s_n <- s0
#   f_n <- func(splitvar = train_cc$Loan_amount1 < s_n, ...)
#   while(f_n == 0){
#     s_n <- abs(rnorm(1, s_n, 2000))
#     f_n <- func(splitvar = train_cc$Loan_amount1 < s_n, ...)
#   }
#   s_b <- s_c <- s_n
#   f_b <- f_c <- f_n
#   message("It\tBest\t\tCurrent\t\tNeigh\t\tTemp\t\tS_Current")
#   message(sprintf("%i\t%.7f\t%.7f\t%.7f\t%.7f\t%.7f", 0L, f_b, f_c, f_n, 1, s_c))
#   
#   for (k in 1:niter) {     
#     Temp <- (1 - step)^k
#     # consider a random neighbor
#     repeat{
#       s_n <- rnorm(1, s_c, 2000)
#       f_n <- func(splitvar = train_cc$Loan_amount1 < s_n, ...)
#       if(s_n > s0 && f_n > 0){
#         break
#       }
#     }
#     
#     # update current state
#     if (f_n > f_c || runif(1, 0, 1) < exp(-(f_n - f_c) / Temp)) {
#       s_c <- s_n
#       f_c <- f_n
#     }
#     # update best state
#     if (f_n > f_b) {
#       s_b <- s_n
#       f_b <- f_n         
#     }
#     #message(sprintf("%i\t%.7f\t%.7f\t%.7f\t%.7f\t%.7f", k, f_b, f_c, f_n, Temp, s_c))
#     states = append(states, s_c)
#     if(Temp < 0.005){
#       break
#     }
#   }
#   return(list(iterations = niter, best_value = f_b, best_state = s_b, all_state = states))
# }
# 
# sol <- simulated_annealing(gini_index, classes = train_cc$Ever30plus_n12MTH, s0 = min(train_cc$Loan_amount1))

