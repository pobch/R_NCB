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


# predict test set
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


