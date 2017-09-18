result.count = function(df){
  result = data.frame( number = nrow(df),
                       default = sum(df$Ever30plus_n12MTH == 'Y'),
                       not.default = sum(df$Ever30plus_n12MTH == 'N'),
                       percent = round(sum(df$Ever30plus_n12MTH == 'Y') / nrow(df),2))
  return(result)
}

split.tree = function(df, cond.str){
  
  if(is.null(df$cond)){
    # root node
    left = eval(parse(text = paste0('df[df$', cond.str, ',]')))
    left$cond = cond.str
    left$layer = 0
    left$left.right = 'left'
    
    right = eval(parse(text = paste0('df[!(df$', cond.str, '),]')))
    right$cond = paste0('!',cond.str)
    right$layer = 0
    right$left.right = 'right'
    
  } else {
    # other nodes
    left = eval(parse(text = paste0('df[df$', cond.str, ',]')))
    left$cond = paste0(unique(df$cond), ', ', cond.str) 
    left$layer = left$layer + 1
    left$left.right = paste0(unique(df$left.right), ', ', 'left') 
    
    right = eval(parse(text = paste0('df[!(df$', cond.str, '),]')))
    right$cond = paste0(unique(df$cond), ', !', cond.str)
    right$layer = right$layer + 1
    right$left.right = paste0(unique(df$left.right), ', ', 'right')
    
  }
  
  return(list(left = left, right = right))
}


del_cols3 = append(del_cols2, 'AMOUNT_PAST_DUE')
del_cols4 = names(train_cc)[!(names(train_cc) %in% c('Ever30plus_n12MTH'))]


################################# HOW TO SPLIT :
tmp = split.tree(train_cc, "ACCOUNT_STATUS_CODE == 'N'")
result.count(tmp$left)
tmp2 = split.tree(tmp$right, "EverXplus_6MTH == 'N'")
result.count(tmp2$right)
tmp3 = split.tree(tmp2$right, "Amount_owe1 <= 24000")
result.count(tmp3$right)
##################################




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




