library(dplyr)

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
gini.num = all_gini_num(train_cc, 'Ever30plus_n12MTH', col.num)
gini.cat = all_gini_cat(train_cc, 'Ever30plus_n12MTH', col.cat)

# Plot:
ggplot(data = subset(gini.num, feature.name == 'DATE_ACCOUNT_OPENED'), aes(x=threshold.date, y=gini.score)) + 
  geom_line()

ggplot(data = subset(gini.num, feature.name == 'Loan_amount1'), aes(x=threshold.num, y=gini.score)) + 
  geom_line() +
  coord_cartesian(xlim = c(-100, 500000))


# evaluate gini score:
conclude = gini.num %>% 
  bind_rows(gini.cat) %>% 
  group_by(feature.name) %>% 
  mutate(the.rank = rank(-gini.score, ties.method = 'random')) %>% 
  filter(the.rank == 1) %>% 
  select(-the.rank) %>% 
  arrange(desc(gini.score))






