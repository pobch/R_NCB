library(combinat)
library(dplyr)

# choose max gini !!!!
gini_index = function(classes,splitvar = NULL) {
  if (is.null(splitvar)) {
    base_prob = table(classes)/length(classes)
    return(sum(base_prob**2))
  }
  if(length(table(splitvar)) < 2){
    return(NA)
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Node_Gini <- sum(crossprob[,1]**2)
  Yes_Node_Gini <- sum(crossprob[,2]**2)
  # weight gini by base_prob:
  return(sum(base_prob * c(No_Node_Gini,Yes_Node_Gini)))
}

#------------------ test : import
setwd('C:\\My Jobs\\gini')
#setwd('D:\\R_codes\\Mortgage')
df = read.csv("data_banknote_authentication.txt", header = FALSE) #df$V5 is the target variable

#make up data:
df$V4.1 = sample(c('Y','N','O','P','U','U','Y'), 1372, replace = T)
df$V4.2 = sample(c('Y','N','N'), 1372, replace = T)
v5 = df$V5
df$V5 = NULL
df$V5 = v5

#-------THE ANSWER (USE AS CONFIRMATION)---------------------------

gini_index(df$V5, df$V1 < 1.7)

# find best gini index of V1
for(i in 1:nrow(df)){
  df$V1gini[i] = gini_index(df$V5, df$V1 < df$V1[i])
}
which.max(df$V1gini) # ANS:284

#-------PROCESSING-----------------------
all_gini_num = function(df, target_col_name, features_col_range) {
  for(i in features_col_range){
    if(is.numeric(df[[i]])){
      print(paste0('number of NA in ', names(df)[i], ' = ', sum(is.na(df[i]))))
      df[paste0(names(df)[i], '_gini')] = apply(df, 1, function(x) { gini_index(df[[target_col_name]], df[i] < x[i] ) })
    } else {
      stop(paste0(names(df)[i],' is not numeric'))
    }
  }
  return(df)
}


all_gini_cat = function(df, target_col_name, features_col_range){
  cat.gini = data.frame(feature.name = character(),
                        group1 = character(),
                        group2 = character(),
                        gini.score = numeric(),
                        stringsAsFactors = F)
  for(i in features_col_range){
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
  }
  return(cat.gini)
}

# ----------------- TEST :
df2 = all_gini_num(df, 'V5', 1:5)
df3 = all_gini_cat(df, 'V5', 5:6)

for(i in 7:11){
  print(max(df2[i], na.rm = T))
}
df3 %>% group_by(feature.name) %>% mutate(the.rank = rank(-gini.score, ties.method = 'random')) %>%  filter(the.rank == 1) %>% select(-the.rank)


#---------------------------------------
library(rpart)
library(rpart.plot)
mod1 = rpart(V5 ~ V4.1 + V4.2, data = df, method ='class', cp = 0.001)
prp(mod1)
