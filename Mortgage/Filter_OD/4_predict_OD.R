build.prop = function(df) {
  df$row.num = seq(1:nrow(df))
  df$bucket = NA
  df$probY = NA
  
  l = df[df$ACCOUNT_STATUS_CODE == 'N', ]
  prob = prop.table(table(l$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% l$row.num, 'l', df$bucket)
  df$probY = ifelse(df$row.num %in% l$row.num, prob[2], df$probY)
  
  rlr = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
             df$Loan_amount2 <= 1.5*10^6 &
             df$Number_of_1plus > 2 , ]
  prob = prop.table(table(rlr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rlr$row.num, 'rlr', df$bucket)
  df$probY = ifelse(df$row.num %in% rlr$row.num, prob[2], df$probY)
  
  rlll = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
              df$Loan_amount2 <= 1.5*10^6 &
              df$Number_of_1plus <= 2 &
              df$AVG_Utilization_6MTH <= 0.87, ]
  prob = prop.table(table(rlll$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rlll$row.num, 'rlll', df$bucket)
  df$probY = ifelse(df$row.num %in% rlll$row.num, prob[2], df$probY)
  
  rllrl = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
               df$Loan_amount2 <= 1.5*10^6 &
               df$Number_of_1plus <= 2 &
               df$AVG_Utilization_6MTH > 0.87 &
               df$ALL_0_below6Mth == 'Y', ]
  prob = prop.table(table(rllrl$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rllrl$row.num, 'rllrl', df$bucket)
  df$probY = ifelse(df$row.num %in% rllrl$row.num, prob[2], df$probY)
  
  rllrr = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
               df$Loan_amount2 <= 1.5*10^6 &
               df$Number_of_1plus <= 2 &
               df$AVG_Utilization_6MTH > 0.87 &
               df$ALL_0_below6Mth == 'N', ]
  prob = prop.table(table(rllrr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rllrr$row.num, 'rllrr', df$bucket)
  df$probY = ifelse(df$row.num %in% rllrr$row.num, prob[2], df$probY)
  
  rrll = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
              df$Loan_amount2 > 1.5*10^6 &
              df$Loan_amount2 <= 3.1*10^6 &
              df$EverXplus_6MTH == 'Y', ]
  prob = prop.table(table(rrll$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rrll$row.num, 'rrll', df$bucket)
  df$probY = ifelse(df$row.num %in% rrll$row.num, prob[2], df$probY)
  
  rrlr = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
              df$Loan_amount2 > 1.5*10^6 &
              df$Loan_amount2 <= 3.1*10^6 &
              df$EverXplus_6MTH == 'N', ]
  prob = prop.table(table(rrlr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rrlr$row.num, 'rrlr', df$bucket)
  df$probY = ifelse(df$row.num %in% rrlr$row.num, prob[2], df$probY)
  
  rrrl = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
              df$Loan_amount2 > 1.5*10^6 &
              df$Loan_amount2 > 3.1*10^6 &
              df$EverXplus_6MTH == 'Y', ]
  prob = prop.table(table(rrrl$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rrrl$row.num, 'rrrl', df$bucket)
  df$probY = ifelse(df$row.num %in% rrrl$row.num, prob[2], df$probY)
  
  rrrrl = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
               df$Loan_amount2 > 1.5*10^6 &
               df$Loan_amount2 > 3.1*10^6 &
               df$EverXplus_6MTH == 'N' &
               df$MOB1 <= 4, ]
  prob = prop.table(table(rrrrl$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rrrrl$row.num, 'rrrrl', df$bucket)
  df$probY = ifelse(df$row.num %in% rrrrl$row.num, prob[2], df$probY)
  
  rrrrr = df[df$ACCOUNT_STATUS_CODE %in% c('A', 'C') &
               df$Loan_amount2 > 1.5*10^6 &
               df$Loan_amount2 > 3.1*10^6 &
               df$EverXplus_6MTH == 'N' &
               df$MOB1 > 4, ]
  prob = prop.table(table(rrrrr$Ever30plus_n12MTH))
  df$bucket = ifelse(df$row.num %in% rrrrr$row.num, 'rrrrr', df$bucket)
  df$probY = ifelse(df$row.num %in% rrrrr$row.num, prob[2], df$probY)
  
  
  return(df)
}


# predict test set
train.prob = build.prop(train_mg)
test.prob = build.prop(test_mg)
test.prob$probY = NULL
train.prob = train.prob %>% 
  select(bucket, probY) %>% 
  distinct(bucket, .keep_all = T)
test.prob = test.prob %>% 
  left_join(train.prob, by = 'bucket') %>% 
  select(Ever30plus_n12MTH, bucket, probY)
test.prob$Ever30plus_n12MTH = ifelse(test.prob$Ever30plus_n12MTH == 'N', 0, 1)  

# ROC and AUC
predROC = prediction(test.prob$probY, test.prob$Ever30plus_n12MTH)
perf = performance(predROC, 'tpr', 'fpr')
plot(perf)
auc = as.numeric(performance(predROC,'auc')@y.values)

# Gini
gini = 2*auc -1 # 0.4454045

# C stat
cstat = auc

# KS
ks.test(test.prob[test.prob$Ever30plus_n12MTH == 1, ]$probY, test.prob[test.prob$Ever30plus_n12MTH == 0, ]$probY) # D = 0.39702, p-value < 2.2e-16

# Rank ordering
tmp = test.prob %>% group_by(probY) %>% filter(Ever30plus_n12MTH == 1) %>% summarise(prop1 = n() / nrow(test.prob))
ggplot(data = tmp, aes(x = probY, y = prop1)) + geom_line(size = 1, col = 'darkred')


