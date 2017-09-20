result.count = function(df){
  result = data.frame( number = nrow(df),
                       default = sum(df$Ever30plus_n12MTH == 'Y'),
                       not.default = sum(df$Ever30plus_n12MTH == 'N'),
                       percent = round(sum(df$Ever30plus_n12MTH == 'Y') / nrow(df),2))
  return(result)
}
del_cols3 = append(del_cols2, 'AMOUNT_PAST_DUE')
del_cols4 = names(train_mg)[!(names(train_mg) %in% c('Ever30plus_n12MTH'))]




# root node: ACCOUNT_STATUS_CODE
l = train_mg[train_mg$ACCOUNT_STATUS_CODE == 'N', ]
r = train_mg[train_mg$ACCOUNT_STATUS_CODE %in% c('A', 'C'), ]
result.count(l)
# number default not.default percent
# 134      33         101    0.25
result.count(r)
# number default not.default percent
# 7703     323        7380    0.04


# root, right : next split by Loan_amount2
all.gini = all_gini(r, 117, 1:116, del_cols2)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)
rl = r[r$Loan_amount2 <= 1.5*10^6, ]
rr = r[r$Loan_amount2 > 1.5*10^6, ]
result.count(rl)
# number default not.default percent
# 5197     176        5021    0.03
result.count(rr)
# number default not.default percent
# 2506     147        2359    0.06


# root, right, right : next split by Loan_amount2
all.gini = all_gini(rr, 117, 1:116, del_cols4)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)
rrl = rr[rr$Loan_amount2 <= 3.1*10^6, ]
rrr = rr[rr$Loan_amount2 > 3.1*10^6, ]
result.count(rrl)
# number default not.default percent
# 1571      80        1491    0.05
result.count(rrr)
# number default not.default percent
# 935      67         868    0.07


# root, right, right, right : next split by EverXplus_6MTH
all.gini = all_gini(rrr, 117, 1:116, del_cols4)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)
rrrl = rrr[rrr$EverXplus_6MTH == 'Y', ]
rrrr = rrr[rrr$EverXplus_6MTH == 'N', ]
result.count(rrrl)
# number default not.default percent
# 98      46          52    0.47
result.count(rrrr)
# number default not.default percent
# 837      21         816    0.03


# root, right, right, right, right : next split by MOB1
all.gini = all_gini(rrrr, 117, 1:116, del_cols4)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)
rrrrl = rrrr[rrrr$MOB1 <= 4, ]
rrrrr = rrrr[rrrr$MOB1 > 4, ]
result.count(rrrrl)
# number default not.default percent
#   62       6          56     0.1
result.count(rrrrr)
# number default not.default percent
#  775      15         760    0.02


#------------------------------------------------------

# root, right, right, left : next split by EverXplus_6MTH
all.gini = all_gini(rrl, 117, 1:116, del_cols4)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)
rrll = rrl[rrl$EverXplus_6MTH == 'Y', ]
rrlr = rrl[rrl$EverXplus_6MTH == 'N', ]
result.count(rrll)
# number default not.default percent
# 123      50          73    0.41
result.count(rrlr)
# number default not.default percent
# 1448      30        1418    0.02


#---------------------------------------------------

# root, right, left : next split by Number_of_1plus
all.gini = all_gini(rl, 117, 1:116, del_cols2)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)
rll = rl[rl$Number_of_1plus <= 2, ]
rlr = rl[rl$Number_of_1plus > 2, ]
result.count(rll)
# number default not.default percent
# 5021     123        4898    0.02
result.count(rlr)
# number default not.default percent
# 176      53         123     0.3


# -------------------------------------------------


# root, right, left, left : next split by AVG_Utilization_6MTH
all.gini = all_gini(rll, 117, 1:116, del_cols3)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)
rlll = rll[rll$AVG_Utilization_6MTH <= 0.87, ]
rllr = rll[rll$AVG_Utilization_6MTH > 0.87, ]
result.count(rlll)
# number default not.default percent
# 3244      47        3197    0.01
result.count(rllr)
# number default not.default percent
# 1777      76        1701    0.04


# root, right, left, left, right : next split by ALL_0_below6Mth
all.gini = all_gini(rllr, 117, 1:116, del_cols4)
all.gini = all.gini %>% 
  mutate(the.rank = rank(gini)) %>% 
  arrange(gini)
rllrl = rllr[rllr$ALL_0_below6Mth == 'Y', ]
rllrr = rllr[rllr$ALL_0_below6Mth == 'N', ]
result.count(rllrl)
# number default not.default percent
# 1695      58        1637    0.03
result.count(rllrr)
# number default not.default percent
#  82      18          64    0.22



################################## END #######################################


tt = data.frame()
result = data.frame()
for(x in unique(lll$MOB1)){
  test = thres.changer(tt, lll, 'Ever30plus_n12MTH', 'MOB1', x)
  result = bind_rows(result, test)
}
result %>% 
  ggplot(aes(x = threshold.num, y = gini)) + 
  geom_line() + 
  ylim(min(result$gini), -0.77) + 
  scale_x_continuous(breaks = seq(0,50, 2), limits = c(5, max(result$threshold.num))) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1)) + 
  geom_vline(xintercept = c(28,30))




