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

# ---------------------------------- Call functions:
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
              'Delay_Payment_code6',
              'MINIMUM_PERCENT_PAYMENT')
all.gini = all_gini(train_mg, 117, 1:116, del_cols2)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

# -------------------------------- How to choose a new threshold:
test = thres.changer(all.gini, train_mg, 'Ever30plus_n12MTH', 'Utilization1', 0.7)
test %>% arrange(gini)



