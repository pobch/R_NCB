setwd('D:\\SAS_data\\Chk_data')
library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(caTools)
library(ROCR)
library(ROSE)
library(GenSA)
library(combinat)


############################### ALL FUNCTIONS ###########################################
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

num.gini = function(df, target_num, predictor_num){
  print(paste0('processing ', names(df)[predictor_num], ', NA = ', sum(is.na(df[predictor_num]))))
  sa.score = simA2(df[[target_num]], df[[predictor_num]])
  result = data.frame(feature.name = names(df)[predictor_num],
                      threshold.num = sa.score$par,
                      gini = sa.score$value,
                      stringsAsFactors = F)
  return(result)
}

num.gini.delcols = function(df, target_num, predictor_num){
  print(paste0('processing ', names(df)[predictor_num], ', NA = ', sum(is.na(df[predictor_num]))))
  tmp = data.frame(feature.name = names(df)[predictor_num],
                   threshold.num = unique(df[[predictor_num]]),
                   gini = sapply(unique(df[[predictor_num]]), gini_index2, classes = df[[target_num]], var = df[[predictor_num]]),
                   stringsAsFactors = F)
  result = tmp[which.min(tmp$gini),]
  return(result)
}

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



############################### IMPORT MORTGAGE DATA ###########################################

df = read_csv("MG_ALL_18MTH.csv", 
              col_types = cols(
                SYSTEM_ID = col_character(),
                TRADE_SERIAL = col_character(),
                INSTALLMENT_FREQUENCY = col_character(),
                ACCOUNT_STATUS = col_character(),
                DELAY_PAYMENT_CODE = col_character(),
                Delay_Payment_code_n1 = col_character(),
                Delay_Payment_code_n2 = col_character(),
                Delay_Payment_code_n3 = col_character(),
                Delay_Payment_code_n4 = col_character(),
                Delay_Payment_code_n5 = col_character(),
                Delay_Payment_code_n6 = col_character(),
                Delay_Payment_code_n7 = col_character(),
                Delay_Payment_code_n8 = col_character(),
                Delay_Payment_code_n9 = col_character(),
                Delay_Payment_code_n10 = col_character(),
                Delay_Payment_code_n11 = col_character(),
                All_Payment_code_n = col_character(),
                OWNERSHIP_INDICATOR = col_character(),
                DATE_ACCOUNT_CLOSED = col_date(format = '%d%b%Y'),
                REPORT_DATE = col_date(format = '%d%b%Y'),
                DATE_ACCOUNT_OPENED = col_date(format = '%d%b%Y'),
                DATE_LAST_PAYMENT = col_date(format = '%d%b%Y'),
                DEFAULT_DATE = col_date(format = '%d%b%Y'),
                DATE_LAST_RESTRUCTURED = col_date(format = '%d%b%Y')
              ))

glimpse(df)


# reorder columns
df = df[, c("SYSTEM_ID", "TRADE_SERIAL", "ID", "CUSTOMER_TYPE", "OWNERSHIP_INDICATOR", 
            "REPORT_DATE", "DATE_ACCOUNT_OPENED", "MOB1", "DATE_ACCOUNT_CLOSED",
            "DATE_LAST_PAYMENT",  "DEFAULT_DATE", "DATE_LAST_RESTRUCTURED", 
            "INSTALLMENT_FREQUENCY", "INSTALLMENT_NUMBER_PAYMENT", "INSTALLMENT_AMOUNT", "AMOUNT_PAST_DUE",
            "ACCOUNT_TYPE",   
            "ACCOUNT_STATUS", "TYPE_OF_CREDIT_CARD", "MINIMUM_PERCENT_PAYMENT",
            "Delay_Payment_code1", "Delay_Payment_code2", "Delay_Payment_code3", "Delay_Payment_code4",
            "Delay_Payment_code5", "Delay_Payment_code6", "All_Payment_code",
            "Loan_amount1", "Loan_amount2", "Loan_amount3", "Loan_amount4", "Loan_amount5","Loan_amount6", 
            "Amount_owe1", "Amount_owe2", "Amount_owe3", "Amount_owe4", "Amount_owe5", "Amount_owe6",  
            "Utilization1", "Utilization2", "Utilization3", "Utilization4", 
            "Utilization5", "Utilization6", "AVG_Utilization_3MTH", "AVG_Utilization_6MTH", 
            "Amount_Owe_Momentum1", "Amount_Owe_Momentum2", "Amount_Owe_Momentum3", 
            "Amount_Owe_Momentum4", "Amount_Owe_Momentum5", "Utilization_Momentum1", 
            "Utilization_Momentum2", "Utilization_Momentum3", "Utilization_Momentum4", 
            "Utilization_Momentum5",  "All_0_6Mth", "ALL_0_below6Mth", 
            "Ever30plus_6MTH", "Ever30plus_5MTH", "Ever30plus_4MTH", "Ever30plus_3MTH", 
            "Ever60plus_6MTH", "Ever60plus_5MTH", "Ever60plus_4MTH", "Ever60plus_3MTH", 
            "EverXplus_6MTH", "EverXplus_5MTH", "EverXplus_4MTH", "EverXplus_3MTH", 
            "Momentum1", "Momentum2", "Momentum3", "Momentum4", "Momentum5", 
            "Min_Momentum", "Max_Momentum", "Number_of_1", "Number_of_1plus", 
            "Number_of_2", "Number_of_2plus", "Number_of_3plus", "ACCOUNT_STATUS_CODE", 
            "Number_OF_Term", "Num_OF_Y", "Num_OF_N", "Count_Y", "Momentum1_OD", 
            "Momentum2_OD", "Momentum3_OD", "Momentum4_OD", "Momentum5_OD", 
            "Min_Momentum_OD", "Max_Momentum_OD", "Num_PL", "Num_CC", "Num_HP", 
            "Num_HL", "Num_OD", "Num_OTH_HP", "Num_CL", "Num_COL", "Num_RD", 
            "Num_OL", "Num_A_PL", "Num_A_CC", "Num_A_HP", "Num_A_HL", "Num_A_OD", 
            "Num_A_OTH_HP", "Num_A_CL", "Num_A_COL", "Num_A_RD", "Num_A_OL", 
            "Loan_amount", "Amount_owe", "DELAY_PAYMENT_CODE", "Utilization", 
            "Loan_amount_n1", "Amount_owe_n1", "Delay_Payment_code_n1", "Utilization_n1", 
            "Loan_amount_n2", "Amount_owe_n2", "Delay_Payment_code_n2", "Utilization_n2", 
            "Loan_amount_n3", "Amount_owe_n3", "Delay_Payment_code_n3", "Utilization_n3", 
            "Loan_amount_n4", "Amount_owe_n4", "Delay_Payment_code_n4", "Utilization_n4", 
            "Loan_amount_n5", "Amount_owe_n5", "Delay_Payment_code_n5", "Utilization_n5", 
            "Loan_amount_n6", "Amount_owe_n6", "Delay_Payment_code_n6", "Utilization_n6", 
            "Loan_amount_n7", "Amount_owe_n7", "Delay_Payment_code_n7", "Utilization_n7", 
            "Loan_amount_n8", "Amount_owe_n8", "Delay_Payment_code_n8", "Utilization_n8", 
            "Loan_amount_n9", "Amount_owe_n9", "Delay_Payment_code_n9", "Utilization_n9", 
            "Loan_amount_n10", "Amount_owe_n10", "Delay_Payment_code_n10", 
            "Utilization_n10", "Loan_amount_n11", "Amount_owe_n11", "Delay_Payment_code_n11", 
            "Utilization_n11", "All_Payment_code_n", "AVG_Utilization_n3MTH", 
            "AVG_Utilization_n6MTH", "AVG_Utilization_n9MTH", "AVG_Utilization_n12MTH", 
            "Amount_Owe_Momentum_n1", "Amount_Owe_Momentum_n2", "Amount_Owe_Momentum_n3", 
            "Amount_Owe_Momentum_n4", "Amount_Owe_Momentum_n5", "Amount_Owe_Momentum_n6", 
            "Amount_Owe_Momentum_n7", "Amount_Owe_Momentum_n8", "Amount_Owe_Momentum_n9", 
            "Amount_Owe_Momentum_n10", "Amount_Owe_Momentum_n11", "Utilization_Momentum_n1", 
            "Utilization_Momentum_n2", "Utilization_Momentum_n3", "Utilization_Momentum_n4", 
            "Utilization_Momentum_n5", "Utilization_Momentum_n6", "Utilization_Momentum_n7", 
            "Utilization_Momentum_n8", "Utilization_Momentum_n9", "Utilization_Momentum_n10", 
            "Utilization_Momentum_n11", "ALL_0_n12MTh", "ALL_0_below_n12Mth", 
            "EverXplus_n12MTH", "Ever30plus_n12MTH", "Ever60plus_n12MTH", 
            "Number_of_1_n12MTH", "Number_of_1plus_n12MTH", "Number_of_2_n12MTH", 
            "Number_of_2plus_n12MTH", "Number_of_3plus_n12MTH", "max_delay", 
            "Momentum_n1", "Momentum_n2", "Momentum_n3", "Momentum_n4", "Momentum_n5", 
            "Momentum_n6", "Momentum_n7", "Momentum_n8", "Momentum_n9", "Momentum_n10", 
            "Momentum_n11", "Min_Momentum_n", "Max_Momentum_n")]

# ----------------- DELETED : 

# df = subset(df, !is.na(Loan_amount1) &
#                !is.na(Loan_amount2) &
#                !is.na(Loan_amount3) &
#                !is.na(Loan_amount4) &
#                !is.na(Loan_amount5) &
#                !is.na(Loan_amount6) &
#                !is.na(Amount_owe1) &
#                !is.na(Amount_owe2) &
#                !is.na(Amount_owe3) &
#                !is.na(Amount_owe4) &
#                !is.na(Amount_owe5) &
#                !is.na(Amount_owe6))

#---------------------------------------------

# change type :
for(i in c("Delay_Payment_code1", "Delay_Payment_code2", "Delay_Payment_code3", "Delay_Payment_code4", "Delay_Payment_code5", "Delay_Payment_code6")){
  df[[i]] = ifelse(df[[i]] == 'F', '10', 
                   ifelse(df[[i]] == 'Y', '1',
                          ifelse(df[[i]] == 'N', '0', df[[i]])))
  df[[i]] = as.numeric(df[[i]])
}


# add new vars
df$nUtil123 = apply(df[, c('Utilization1', 'Utilization2', 'Utilization3')], 1, function(x) {sum(!is.na(x))}) 
df$stdUtil123 = apply(df[ , c('Utilization1', 'Utilization2', 'Utilization3')], 1, function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)} )

df$nUtil456 = apply(df[, c('Utilization4', 'Utilization5', 'Utilization6')], 1, function(x) {sum(!is.na(x))})
df$stdUtil456 = apply(df[ , c('Utilization4', 'Utilization5', 'Utilization6')], 1, function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)})

df$nUtil1to6 = apply(df[, c('Utilization1', 'Utilization2', 'Utilization3', 'Utilization4', 'Utilization5', 'Utilization6')], 1, function(x) {sum(!is.na(x))})
df$stdUtil1to6 = apply(df[ , c('Utilization1', 'Utilization2', 'Utilization3', 'Utilization4', 'Utilization5', 'Utilization6')], 1, function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)})

df$nloans = apply(df[, 96:105], 1, sum)
df$nactiveloans = apply(df[, 106:115], 1, sum)

# Check
table(df$nUtil1to6 == (df$nUtil123 + df$nUtil456))

# add new vars
df$max_util_momentum = apply(df[, c('Utilization_Momentum1', 'Utilization_Momentum2', 'Utilization_Momentum3',
                                    'Utilization_Momentum4', 'Utilization_Momentum5')], 1,
                             max, na.rm = T)

df$min_util_momentum = apply(df[, c('Utilization_Momentum1', 'Utilization_Momentum2', 'Utilization_Momentum3',
                                    'Utilization_Momentum4', 'Utilization_Momentum5')], 1,
                             min, na.rm = T)


#----------------------- count number of NA in each column:
sapply(df, function(x) {sum(is.na(x))})
# or
colSums(is.na(df))


#------------------------ Replace missing value (Pob's version):
df$INSTALLMENT_FREQUENCY = ifelse(is.na(df$INSTALLMENT_FREQUENCY) & df$ACCOUNT_TYPE == '04',
                                  '0',
                                  ifelse(is.na(df$INSTALLMENT_FREQUENCY) & df$ACCOUNT_TYPE == '22',
                                         '3',
                                         df$INSTALLMENT_FREQUENCY)
)
df$TYPE_OF_CREDIT_CARD = ifelse(is.na(df$TYPE_OF_CREDIT_CARD),
                                '0',
                                df$TYPE_OF_CREDIT_CARD)
df$MINIMUM_PERCENT_PAYMENT = ifelse(is.na(df$MINIMUM_PERCENT_PAYMENT),
                                    0,
                                    df$MINIMUM_PERCENT_PAYMENT)
df$is.DeLoOw2.NA = ifelse(is.na(df$Delay_Payment_code2),
                          'Yes',
                          'No') 
df$is.DeLoOw3.NA = ifelse(is.na(df$Delay_Payment_code3),
                          'Yes',
                          'No') 
df$is.DeLoOw4.NA = ifelse(is.na(df$Delay_Payment_code4),
                          'Yes',
                          'No') 
df$is.DeLoOw5.NA = ifelse(is.na(df$Delay_Payment_code5),
                          'Yes',
                          'No') 
df$is.DeLoOw6.NA = ifelse(is.na(df$Delay_Payment_code6),
                          'Yes',
                          'No')   

df$Delay_Payment_code2 = ifelse(is.na(df$Delay_Payment_code2),
                                0,
                                df$Delay_Payment_code2)
df$Delay_Payment_code3 = ifelse(is.na(df$Delay_Payment_code3),
                                0,
                                df$Delay_Payment_code3)
df$Delay_Payment_code4 = ifelse(is.na(df$Delay_Payment_code4),
                                0,
                                df$Delay_Payment_code4)
df$Delay_Payment_code5 = ifelse(is.na(df$Delay_Payment_code5),
                                0,
                                df$Delay_Payment_code5)
df$Delay_Payment_code6 = ifelse(is.na(df$Delay_Payment_code6),
                                0,
                                df$Delay_Payment_code6)
df$Loan_amount2 = ifelse(is.na(df$Loan_amount2),
                         0,
                         df$Loan_amount2)
df$Loan_amount3 = ifelse(is.na(df$Loan_amount3),
                         0,
                         df$Loan_amount3)
df$Loan_amount4 = ifelse(is.na(df$Loan_amount4),
                         0,
                         df$Loan_amount4)
df$Loan_amount5 = ifelse(is.na(df$Loan_amount5),
                         0,
                         df$Loan_amount5)
df$Loan_amount6 = ifelse(is.na(df$Loan_amount6),
                         0,
                         df$Loan_amount6)
df$Amount_owe2 = ifelse(is.na(df$Amount_owe2),
                        0,
                        df$Amount_owe2)
df$Amount_owe3 = ifelse(is.na(df$Amount_owe3),
                        0,
                        df$Amount_owe3)
df$Amount_owe4 = ifelse(is.na(df$Amount_owe4),
                        0,
                        df$Amount_owe4)
df$Amount_owe5 = ifelse(is.na(df$Amount_owe5),
                        0,
                        df$Amount_owe5)
df$Amount_owe6 = ifelse(is.na(df$Amount_owe6),
                        0,
                        df$Amount_owe6)

df$AVG_Utilization_3MTH = ifelse(is.na(df$AVG_Utilization_3MTH),
                                 0,
                                 df$AVG_Utilization_3MTH)
df$AVG_Utilization_6MTH = ifelse(is.na(df$AVG_Utilization_6MTH),
                                 0,
                                 df$AVG_Utilization_6MTH)
df$is.Util1.NA = ifelse(is.na(df$Utilization1),
                        'Yes',
                        'No')
df$is.Util2.NA = ifelse(is.na(df$Utilization2),
                        'Yes',
                        'No')
df$is.Util3.NA = ifelse(is.na(df$Utilization3),
                        'Yes',
                        'No')
df$is.Util4.NA = ifelse(is.na(df$Utilization4),
                        'Yes',
                        'No')
df$is.Util5.NA = ifelse(is.na(df$Utilization5),
                        'Yes',
                        'No')
df$is.Util6.NA = ifelse(is.na(df$Utilization6),
                        'Yes',
                        'No')

df$Utilization1 = ifelse(is.na(df$Utilization1),
                         0,
                         df$Utilization1)
df$Utilization2 = ifelse(is.na(df$Utilization2),
                         0,
                         df$Utilization2)
df$Utilization3 = ifelse(is.na(df$Utilization3),
                         0,
                         df$Utilization3)
df$Utilization4 = ifelse(is.na(df$Utilization4),
                         0,
                         df$Utilization4)
df$Utilization5 = ifelse(is.na(df$Utilization5),
                         0,
                         df$Utilization5)
df$Utilization6 = ifelse(is.na(df$Utilization6),
                         0,
                         df$Utilization6)
df$is.OweMomen1.NA = ifelse(is.na(df$Amount_Owe_Momentum1),
                            'Yes',
                            'No')
df$is.OweMomen2.NA = ifelse(is.na(df$Amount_Owe_Momentum2),
                            'Yes',
                            'No')
df$is.OweMomen3.NA = ifelse(is.na(df$Amount_Owe_Momentum3),
                            'Yes',
                            'No')
df$is.OweMomen4.NA = ifelse(is.na(df$Amount_Owe_Momentum4),
                            'Yes',
                            'No')
df$is.OweMomen5.NA = ifelse(is.na(df$Amount_Owe_Momentum5),
                            'Yes',
                            'No')
df$Amount_Owe_Momentum1 = ifelse(is.na(df$Amount_Owe_Momentum1),
                                 0,
                                 df$Amount_Owe_Momentum1)
df$Amount_Owe_Momentum2 = ifelse(is.na(df$Amount_Owe_Momentum2),
                                 0,
                                 df$Amount_Owe_Momentum2)
df$Amount_Owe_Momentum3 = ifelse(is.na(df$Amount_Owe_Momentum3),
                                 0,
                                 df$Amount_Owe_Momentum3)
df$Amount_Owe_Momentum4 = ifelse(is.na(df$Amount_Owe_Momentum4),
                                 0,
                                 df$Amount_Owe_Momentum4)
df$Amount_Owe_Momentum5 = ifelse(is.na(df$Amount_Owe_Momentum5),
                                 0,
                                 df$Amount_Owe_Momentum5)
df$is.UtilMomen1.NA = ifelse(is.na(df$Utilization_Momentum1),
                             'Yes',
                             'No')
df$is.UtilMomen2.NA = ifelse(is.na(df$Utilization_Momentum2),
                             'Yes',
                             'No')
df$is.UtilMomen3.NA = ifelse(is.na(df$Utilization_Momentum3),
                             'Yes',
                             'No')
df$is.UtilMomen4.NA = ifelse(is.na(df$Utilization_Momentum4),
                             'Yes',
                             'No')
df$is.UtilMomen5.NA = ifelse(is.na(df$Utilization_Momentum5),
                             'Yes',
                             'No')
df$Utilization_Momentum1 = ifelse(is.na(df$Utilization_Momentum1),
                                  0,
                                  df$Utilization_Momentum1)
df$Utilization_Momentum2 = ifelse(is.na(df$Utilization_Momentum2),
                                  0,
                                  df$Utilization_Momentum2)
df$Utilization_Momentum3 = ifelse(is.na(df$Utilization_Momentum3),
                                  0,
                                  df$Utilization_Momentum3)
df$Utilization_Momentum4 = ifelse(is.na(df$Utilization_Momentum4),
                                  0,
                                  df$Utilization_Momentum4)
df$Utilization_Momentum5 = ifelse(is.na(df$Utilization_Momentum5),
                                  0,
                                  df$Utilization_Momentum5)
df$is.Momentum2.NA = ifelse(is.na(df$Momentum2),
                            'Yes',
                            'No')
df$is.Momentum3.NA = ifelse(is.na(df$Momentum3),
                            'Yes',
                            'No')
df$is.Momentum4.NA = ifelse(is.na(df$Momentum4),
                            'Yes',
                            'No')
df$is.Momentum5.NA = ifelse(is.na(df$Momentum5),
                            'Yes',
                            'No')
df$Momentum2 = ifelse(is.na(df$Momentum2),
                      0,
                      df$Momentum2)
df$Momentum3 = ifelse(is.na(df$Momentum3),
                      0,
                      df$Momentum3)
df$Momentum4 = ifelse(is.na(df$Momentum4),
                      0,
                      df$Momentum4)
df$Momentum5 = ifelse(is.na(df$Momentum5),
                      0,
                      df$Momentum5)
df$is.stdUtil123.NA = ifelse(is.na(df$stdUtil123),
                             'Yes',
                             'No')
df$is.stdUtil456.NA = ifelse(is.na(df$stdUtil456),
                             'Yes',
                             'No')
df$is.stdUtil1to6.NA = ifelse(is.na(df$stdUtil1to6),
                              'Yes',
                              'No')
df$stdUtil123 = ifelse(is.na(df$stdUtil123),
                       0,
                       df$stdUtil123)
df$stdUtil456 = ifelse(is.na(df$stdUtil456),
                       0,
                       df$stdUtil456)
df$stdUtil1to6 = ifelse(is.na(df$stdUtil1to6),
                        0,
                        df$stdUtil1to6)

#---------------------------------------------------------


# list all column names:
dput(names(df))

# choose columns for OD and not-OD:
subdf = df[which(df$ACCOUNT_TYPE != '04'), c(
  "All_0_6Mth", "ALL_0_below6Mth", 
  "Ever30plus_6MTH", "Ever30plus_5MTH", "Ever30plus_4MTH", "Ever30plus_3MTH", 
  "Ever60plus_6MTH", "Ever60plus_5MTH", "Ever60plus_4MTH", "Ever60plus_3MTH", 
  "EverXplus_6MTH", "EverXplus_5MTH", "EverXplus_4MTH", "EverXplus_3MTH", 
  "Momentum1", "Momentum2", "Momentum3", "Momentum4", "Momentum5", 
  "Min_Momentum", "Max_Momentum", 
  "Number_of_1", "Number_of_1plus", 
  "Number_of_2", "Number_of_2plus", 
  "Number_of_3plus", 
  
  "SYSTEM_ID", "TRADE_SERIAL",  
  "CUSTOMER_TYPE", "OWNERSHIP_INDICATOR", 
  "REPORT_DATE", "DATE_ACCOUNT_OPENED", "DATE_ACCOUNT_CLOSED",
  "DATE_LAST_PAYMENT",  "DEFAULT_DATE", "DATE_LAST_RESTRUCTURED", 
  "MOB1", 
  "INSTALLMENT_FREQUENCY", "INSTALLMENT_NUMBER_PAYMENT", "INSTALLMENT_AMOUNT", 
  "MINIMUM_PERCENT_PAYMENT", "AMOUNT_PAST_DUE",
  "ACCOUNT_TYPE", "TYPE_OF_CREDIT_CARD", "ACCOUNT_STATUS", "ACCOUNT_STATUS_CODE",
  "Delay_Payment_code1", "Delay_Payment_code2", "Delay_Payment_code3", 
  "Delay_Payment_code4", "Delay_Payment_code5", "Delay_Payment_code6", 
  "All_Payment_code",
  "Loan_amount1", "Loan_amount2", "Loan_amount3", 
  "Loan_amount4", "Loan_amount5", "Loan_amount6", 
  "Amount_owe1", "Amount_owe2", "Amount_owe3", 
  "Amount_owe4", "Amount_owe5", "Amount_owe6",  
  "Utilization1", "Utilization2", "Utilization3", 
  "Utilization4", "Utilization5", "Utilization6", 
  "AVG_Utilization_3MTH", "AVG_Utilization_6MTH", 
  "Amount_Owe_Momentum1", "Amount_Owe_Momentum2", "Amount_Owe_Momentum3", 
  "Amount_Owe_Momentum4", "Amount_Owe_Momentum5", 
  "Utilization_Momentum1", "Utilization_Momentum2", "Utilization_Momentum3", 
  "Utilization_Momentum4", "Utilization_Momentum5", 
  "Num_PL", "Num_CC", "Num_HP", "Num_HL", "Num_OD", "Num_OTH_HP", 
  "Num_CL", "Num_COL", "Num_RD", "Num_OL", 
  "Num_A_PL", "Num_A_CC", "Num_A_HP", "Num_A_HL", "Num_A_OD", 
  "Num_A_OTH_HP", "Num_A_CL", "Num_A_COL", "Num_A_RD", "Num_A_OL", 
  "nloans", "nactiveloans", 
  "nUtil123", "nUtil456", "nUtil1to6",
  "stdUtil123", "stdUtil456", "stdUtil1to6", 
  "max_util_momentum", "min_util_momentum", 
  
  "is.DeLoOw2.NA", "is.DeLoOw3.NA", "is.DeLoOw4.NA", "is.DeLoOw5.NA", 
  "is.DeLoOw6.NA", "is.Util1.NA", "is.Util2.NA", "is.Util3.NA", 
  "is.Util4.NA", "is.Util5.NA", "is.Util6.NA", "is.OweMomen1.NA", 
  "is.OweMomen2.NA", "is.OweMomen3.NA", "is.OweMomen4.NA", "is.OweMomen5.NA", 
  "is.UtilMomen1.NA", "is.UtilMomen2.NA", "is.UtilMomen3.NA", "is.UtilMomen4.NA", 
  "is.UtilMomen5.NA", "is.Momentum2.NA", "is.Momentum3.NA", "is.Momentum4.NA", 
  "is.Momentum5.NA", "is.stdUtil123.NA", "is.stdUtil456.NA", "is.stdUtil1to6.NA",
  
  "Ever30plus_n12MTH")]

subdf_od = df[which(df$ACCOUNT_TYPE == '04'), c(
  "Number_OF_Term", "Num_OF_Y", "Num_OF_N", "Count_Y", 
  "Momentum1_OD", "Momentum2_OD", "Momentum3_OD", "Momentum4_OD", "Momentum5_OD", 
  "Min_Momentum_OD", "Max_Momentum_OD",
  
  "SYSTEM_ID", "TRADE_SERIAL",  
  "CUSTOMER_TYPE", "OWNERSHIP_INDICATOR", 
  "REPORT_DATE", "DATE_ACCOUNT_OPENED", "DATE_ACCOUNT_CLOSED",
  "DATE_LAST_PAYMENT",  "DEFAULT_DATE", "DATE_LAST_RESTRUCTURED", 
  "MOB1", 
  "INSTALLMENT_FREQUENCY", "INSTALLMENT_NUMBER_PAYMENT", "INSTALLMENT_AMOUNT", 
  "MINIMUM_PERCENT_PAYMENT", "AMOUNT_PAST_DUE",
  "ACCOUNT_TYPE", "TYPE_OF_CREDIT_CARD", "ACCOUNT_STATUS", "ACCOUNT_STATUS_CODE",
  "Delay_Payment_code1", "Delay_Payment_code2", "Delay_Payment_code3", 
  "Delay_Payment_code4", "Delay_Payment_code5", "Delay_Payment_code6", 
  "All_Payment_code",
  "Loan_amount1", "Loan_amount2", "Loan_amount3", 
  "Loan_amount4", "Loan_amount5", "Loan_amount6", 
  "Amount_owe1", "Amount_owe2", "Amount_owe3", 
  "Amount_owe4", "Amount_owe5", "Amount_owe6",  
  "Utilization1", "Utilization2", "Utilization3", 
  "Utilization4", "Utilization5", "Utilization6", 
  "AVG_Utilization_3MTH", "AVG_Utilization_6MTH", 
  "Amount_Owe_Momentum1", "Amount_Owe_Momentum2", "Amount_Owe_Momentum3", 
  "Amount_Owe_Momentum4", "Amount_Owe_Momentum5", 
  "Utilization_Momentum1", "Utilization_Momentum2", "Utilization_Momentum3", 
  "Utilization_Momentum4", "Utilization_Momentum5", 
  "Num_PL", "Num_CC", "Num_HP", "Num_HL", "Num_OD", "Num_OTH_HP", 
  "Num_CL", "Num_COL", "Num_RD", "Num_OL", 
  "Num_A_PL", "Num_A_CC", "Num_A_HP", "Num_A_HL", "Num_A_OD", 
  "Num_A_OTH_HP", "Num_A_CL", "Num_A_COL", "Num_A_RD", "Num_A_OL", 
  "nloans", "nactiveloans", 
  "nUtil123", "nUtil456", "nUtil1to6",
  "stdUtil123", "stdUtil456", "stdUtil1to6", 
  "max_util_momentum", "min_util_momentum", 
  
  "is.DeLoOw2.NA", "is.DeLoOw3.NA", "is.DeLoOw4.NA", "is.DeLoOw5.NA", 
  "is.DeLoOw6.NA", "is.Util1.NA", "is.Util2.NA", "is.Util3.NA", 
  "is.Util4.NA", "is.Util5.NA", "is.Util6.NA", "is.OweMomen1.NA", 
  "is.OweMomen2.NA", "is.OweMomen3.NA", "is.OweMomen4.NA", "is.OweMomen5.NA", 
  "is.UtilMomen1.NA", "is.UtilMomen2.NA", "is.UtilMomen3.NA", "is.UtilMomen4.NA", 
  "is.UtilMomen5.NA", "is.Momentum2.NA", "is.Momentum3.NA", "is.Momentum4.NA", 
  "is.Momentum5.NA", "is.stdUtil123.NA", "is.stdUtil456.NA", "is.stdUtil1to6.NA",
  
  "Ever30plus_n12MTH")]

################################ SPLIT TRAIN/TEST FOR CC ###########################################
df2 = select(subdf, -SYSTEM_ID, -TRADE_SERIAL, -CUSTOMER_TYPE, -All_Payment_code)

subdf_cc = df2 %>% 
  subset(ACCOUNT_TYPE == '22') %>% 
  select(-INSTALLMENT_NUMBER_PAYMENT, -INSTALLMENT_AMOUNT, -ACCOUNT_TYPE,
         -Num_PL, -Num_HP, -Num_HL, -Num_OD, -Num_OTH_HP, -Num_CL,
         -Num_COL, -Num_RD, -Num_OL, -Num_A_PL, -Num_A_HP, -Num_A_HL, 
         -Num_A_OD, -Num_A_OTH_HP, -Num_A_CL, -Num_A_COL, -Num_A_RD, -Num_A_OL,
         -nloans, -nactiveloans)

set.seed(123)
spl = sample.split(subdf_cc$Ever30plus_n12MTH, SplitRatio = 0.7)
train_cc = subset(subdf_cc, spl == T)
test_cc = subset(subdf_cc, spl == F)


################################ CART MODEL ###########################################

result.count = function(df){
  result = data.frame( number = nrow(df),
                       default = sum(df$Ever30plus_n12MTH == 'Y'),
                       not.default = sum(df$Ever30plus_n12MTH == 'N'),
                       percent = round(sum(df$Ever30plus_n12MTH == 'Y') / nrow(df),2))
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
del_cols3 = append(del_cols2, 'AMOUNT_PAST_DUE')
del_cols4 = names(train_cc)[!(names(train_cc) %in% c('Ever30plus_n12MTH'))]


# root node: ACCOUNT_STATUS_CODE
left1 = train_cc[train_cc$ACCOUNT_STATUS_CODE == 'N', ]
right1 = train_cc[train_cc$ACCOUNT_STATUS_CODE %in% c('A', 'C'), ]
result.count(left1)
# number default not.default percent
# 555     108         447    0.19
result.count(right1)
# number default not.default percent
# 31548     605       30943    0.02





# right >>> split by EverXplus_6MTH
all.gini = all_gini(right1, 115, 1:114, del_cols3)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

rl = right1[right1$EverXplus_6MTH == 'N', ]
rr = right1[right1$EverXplus_6MTH == 'Y', ]
result.count(rl)
# number default not.default percent
# 31179     529       30650    0.02
result.count(rr)
# number default not.default percent
# 369      76         293    0.21





# right >> right >>>> split by Amount_owe1
all.gini = all_gini(rr, 115, 1:114, del_cols3)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

rrl = rr[rr$Amount_owe1 <= 24000, ]
rrr = rr[rr$Amount_owe1 > 24000, ]
result.count(rrl)
# number default not.default percent
# 150      14         136    0.09
result.count(rrr)
# number default not.default percent
# 219      62         157    0.28







# right >> right >> left >>> split by Number_of_1plus 
all.gini = all_gini(rrl, 115, 1:114, del_cols4)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

rrll = rrl[rrl$Number_of_1plus < 2 , ] # **********LAST NODE********
rrlr = rrl[rrl$Number_of_1plus >= 2 , ] # **********LAST NODE********
result.count(rrll)
# number default not.default percent
# 90       2          88    0.02
result.count(rrlr)
# number default not.default percent
# 60      12          48     0.2


#---------------------

# right >> right >> right >>>> split by Num_CC
all.gini = all_gini(rrr, 115, 1:114, del_cols4)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

rrrl = rrr[rrr$Num_CC <= 1 , ] # ********* LAST NODE ********
rrrr = rrr[rrr$Num_CC > 1 , ]
result.count(rrrl)
# number default not.default percent
# 26      11          15      0.42
result.count(rrrr)
# number default not.default percent
# 193      51         142    0.26






# right >> right >> right >> right >>>>> split by stdUtil123
all.gini = all_gini(rrrr, 115, 1:114, del_cols4)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

rrrrl = rrrr[rrrr$stdUtil123 < 0.01, ] # ************** LAST NODE **********
rrrrr = rrrr[rrrr$stdUtil123 >= 0.01, ] # ************** LAST NODE **********
result.count(rrrrl)
# number default not.default percent
# 23       2          21    0.09
result.count(rrrrr)
# number default not.default percent
# 170      49         121    0.29




# -------------------------------------------

# right >> left >>>> split by Utilization1
all.gini = all_gini(rl, 115, 1:114, del_cols3)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

rll = rl[rl$Utilization1 < 0.9, ]
rlr = rl[rl$Utilization1 >= 0.9, ]
result.count(rll)
# number default not.default percent
# 27724     356       27368    0.01
result.count(rlr)
# number default not.default percent
# 3455     173        3282    0.05






# right >> left >> right >>>> split by MOB1
all.gini = all_gini(rlr, 115, 1:114, del_cols3)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

rlrl = rlr[rlr$MOB1 <= 2, ] # **************** LAST NODE ******
rlrr = rlr[rlr$MOB1 > 2, ]
result.count(rlrl)
# number default not.default percent
# 121      13         108    0.11
result.count(rlrr)
# number default not.default percent
# 3334     160        3174    0.05






# right >> left >> righ >> right >>>> split by stdUtil123
all.gini = all_gini(rlrr, 115, 1:114, del_cols3)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

rlrrl = rlrr[rlrr$stdUtil123 < 0.14 ,] 
rlrrr = rlrr[rlrr$stdUtil123 >= 0.14 ,] # ********** LAST NODE *******
result.count(rlrrl)
# number default not.default percent
# 2018     124        1894    0.06
result.count(rlrrr)
# number default not.default percent
# 1316      36        1280    0.03





# right >> left >> righ >> right >> left >>> split by MOB1
all.gini = all_gini(rlrrl, 115, 1:114, del_cols3)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

rlrrll = rlrrl[rlrrl$MOB1 < 24 , ]
rlrrlr = rlrrl[rlrrl$MOB1 >= 24 , ] # ******** LAST NODE *******
result.count(rlrrll)
# number default not.default percent
# 512      51         461     0.1
result.count(rlrrlr)
# number default not.default percent
# 1506      73        1433    0.05





# right >> left >> righ >> right >> left >> left >>> split by MOB1
all.gini = all_gini(rlrrll, 115, 1:114, del_cols4)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

rlrrlll = rlrrll[rlrrll$MOB1 < 7 ,] # ****** LAST NODE *******
rlrrllr = rlrrll[rlrrll$MOB1 >= 7, ] # ******** LAST NODE *******
result.count(rlrrlll)
# number default not.default percent
# 85      12          73    0.14
result.count(rlrrllr)
# number default not.default percent
# 427      39         388    0.09



#-------------------------------

# right >> left >> left >>>> split by Loan_amount1
all.gini = all_gini(rll, 115, 1:114, del_cols3)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

rlll = rll[rll$Loan_amount1 < 18000, ] # ******** LAST NODE *******
rllr = rll[rll$Loan_amount1 >= 18000, ] # ******** LAST NODE *******
result.count(rlll)
# number default not.default percent
# 3235      83        3152    0.03
result.count(rllr)
# number default not.default percent
# 24489     273       24216    0.01




# -------------------------------


# left >>> split by ACCOUNT_STATUS
all.gini = all_gini(left1, 115, 1:114, del_cols3)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

ll = left1[left1$ACCOUNT_STATUS == '40', ]
lr = left1[left1$ACCOUNT_STATUS %in% c('30', '31'), ] # ******** LAST NODE **********
result.count(ll)
# number default not.default percent
# 429      72         357    0.17
result.count(lr)
# number default not.default percent
# 126      36          90    0.29





# left >> left >>>> split by Min_Momentum
all.gini = all_gini(ll, 115, 1:114, del_cols4)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

lll = ll[ll$Min_Momentum < 1 ,]
llr = ll[ll$Min_Momentum >= 1 , ] # *********** LAST NODE ***********
result.count(lll)
# number default not.default percent
# 372      48         324    0.13
result.count(llr)
# number default not.default percent
# 57      24          33    0.42



# left >> left >> left >>>>> split by MOB1
all.gini = all_gini(lll, 115, 1:114, del_cols4)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)

llll = lll[lll$MOB1 < 30,] # *********** LAST NODE ***********
lllr = lll[lll$MOB1 >= 30,] # *********** LAST NODE ***********
result.count(llll)
# number default not.default percent
# 88      17          71    0.19
result.count(lllr)
# number default not.default percent
# 284      31         253    0.11




############################### PREDICT ###########################################
build.prop = function(df) {
  df$row.num = seq(1:nrow(df))
  df$bucket = NA
  df$probY = NA
  
  
  rrll = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
              df$EverXplus_6MTH == 'Y' &
              df$Amount_owe1 <= 24000 &
              df$Number_of_1plus < 2 , ]
  prob = prop.table(table(rrll$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rrll$row.num, 'rrll', df$bucket)
  df$probY = ifelse(df$row.num %in% rrll$row.num, prob[2], df$probY)
  
  
  rrlr = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
              df$EverXplus_6MTH == 'Y' &
              df$Amount_owe1 <= 24000 &
              df$Number_of_1plus >= 2 , ]
  prob = prop.table(table(rrlr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rrlr$row.num, 'rrlr', df$bucket)
  df$probY = ifelse(df$row.num %in% rrlr$row.num, prob[2], df$probY)
  
  
  rrrl = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
              df$EverXplus_6MTH == 'Y' &
              df$Amount_owe1 > 24000 &
              df$Num_CC <= 1, ]
  prob = prop.table(table(rrrl$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rrrl$row.num, 'rrrl', df$bucket)
  df$probY = ifelse(df$row.num %in% rrrl$row.num, prob[2], df$probY)
  
  
  rrrrl = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
               df$EverXplus_6MTH == 'Y' &
               df$Amount_owe1 > 24000 &
               df$Num_CC > 1 &
               df$stdUtil123 < 0.01, ]
  prob = prop.table(table(rrrrl$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rrrrl$row.num, 'rrrrl', df$bucket)
  df$probY = ifelse(df$row.num %in% rrrrl$row.num, prob[2], df$probY)
  
  
  rrrrr = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
               df$EverXplus_6MTH == 'Y' &
               df$Amount_owe1 > 24000 &
               df$Num_CC > 1 &
               df$stdUtil123 >= 0.01, ]
  prob = prop.table(table(rrrrr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rrrrr$row.num, 'rrrrr', df$bucket)
  df$probY = ifelse(df$row.num %in% rrrrr$row.num, prob[2], df$probY)
  
  
  rlll = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
              df$EverXplus_6MTH == 'N' &
              df$Utilization1 < 0.9 &
              df$Loan_amount1 < 18000, ] 
  prob = prop.table(table(rlll$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rlll$row.num, 'rlll', df$bucket)
  df$probY = ifelse(df$row.num %in% rlll$row.num, prob[2], df$probY)
  
  
  rllr = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
              df$EverXplus_6MTH == 'N' &
              df$Utilization1 < 0.9 &
              df$Loan_amount1 >= 18000, ]
  prob = prop.table(table(rllr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rllr$row.num, 'rllr', df$bucket)
  df$probY = ifelse(df$row.num %in% rllr$row.num, prob[2], df$probY)
  
  
  rlrl = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
              df$EverXplus_6MTH == 'N' &
              df$Utilization1 >= 0.9 &
              df$MOB1 <= 2, ]
  prob = prop.table(table(rlrl$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rlrl$row.num, 'rlrl', df$bucket)
  df$probY = ifelse(df$row.num %in% rlrl$row.num, prob[2], df$probY)
  
  
  rlrrr = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
               df$EverXplus_6MTH == 'N' &
               df$Utilization1 >= 0.9 &
               df$MOB1 > 2 &
               df$stdUtil123 >= 0.14, ]
  prob = prop.table(table(rlrrr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rlrrr$row.num, 'rlrrr', df$bucket)
  df$probY = ifelse(df$row.num %in% rlrrr$row.num, prob[2], df$probY)
  
  
  rlrrlr = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
                df$EverXplus_6MTH == 'N' &
                df$Utilization1 >= 0.9 &
                df$MOB1 > 2 &
                df$stdUtil123 < 0.14 &
                df$MOB1 >= 24, ]
  prob = prop.table(table(rlrrlr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rlrrlr$row.num, 'rlrrlr', df$bucket)
  df$probY = ifelse(df$row.num %in% rlrrlr$row.num, prob[2], df$probY)
  
  
  rlrrlll = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
                 df$EverXplus_6MTH == 'N' &
                 df$Utilization1 >= 0.9 &
                 df$MOB1 > 2 &
                 df$stdUtil123 < 0.14 &
                 df$MOB1 < 24 &
                 df$MOB1 < 7, ]
  prob = prop.table(table(rlrrlll$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rlrrlll$row.num, 'rlrrlll', df$bucket)
  df$probY = ifelse(df$row.num %in% rlrrlll$row.num, prob[2], df$probY)
  
  rlrrllr = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
                 df$EverXplus_6MTH == 'N' &
                 df$Utilization1 >= 0.9 &
                 df$MOB1 > 2 &
                 df$stdUtil123 < 0.14 &
                 df$MOB1 < 24 &
                 df$MOB1 >= 7, ]
  prob = prop.table(table(rlrrllr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rlrrllr$row.num, 'rlrrllr', df$bucket)
  df$probY = ifelse(df$row.num %in% rlrrllr$row.num, prob[2], df$probY)
  
  #-------------------
  
  lr = df[df$ACCOUNT_STATUS_CODE == 'N' &
            df$ACCOUNT_STATUS %in% c('30', '31'), ]
  prob = prop.table(table(lr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% lr$row.num, 'lr', df$bucket)
  df$probY = ifelse(df$row.num %in% lr$row.num, prob[2], df$probY)
  
  
  llr = df[df$ACCOUNT_STATUS_CODE == 'N' &
             df$ACCOUNT_STATUS == '40' &
             df$Min_Momentum >= 1, ]
  prob = prop.table(table(llr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% llr$row.num, 'llr', df$bucket)
  df$probY = ifelse(df$row.num %in% llr$row.num, prob[2], df$probY)
  
  
  llll = df[df$ACCOUNT_STATUS_CODE == 'N' &
              df$ACCOUNT_STATUS == '40' &
              df$Min_Momentum < 1 &
              df$MOB1 < 30, ]
  prob = prop.table(table(llll$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% llll$row.num, 'llll', df$bucket)
  df$probY = ifelse(df$row.num %in% llll$row.num, prob[2], df$probY)
  
  lllr = df[df$ACCOUNT_STATUS_CODE == 'N' &
              df$ACCOUNT_STATUS == '40' &
              df$Min_Momentum < 1 &
              df$MOB1 >= 30, ]
  prob = prop.table(table(lllr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% lllr$row.num, 'lllr', df$bucket)
  df$probY = ifelse(df$row.num %in% lllr$row.num, prob[2], df$probY)
  
  return(df)
}

train.prob = build.prop(train_cc)
test.prob = build.prop(test_cc)
test.prob$probY = NULL
train.prob = train.prob %>% 
  select(bucket, probY) %>% 
  distinct(bucket, .keep_all = T)
test.prob = test.prob %>% 
  left_join(train.prob, by = 'bucket') %>% 
  select(Ever30plus_n12MTH, bucket, probY)
test.prob$Ever30plus_n12MTH = ifelse(test.prob$Ever30plus_n12MTH == 'N', 0, 1)  






############################### ROC AND AUC TEST ###########################################
predROC = prediction(test.prob$probY, test.prob$Ever30plus_n12MTH)
perf = performance(predROC, 'tpr', 'fpr')
plot(perf)
as.numeric(performance(predROC,'auc')@y.values)