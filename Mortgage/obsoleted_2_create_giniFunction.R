library(combinat)
library(dplyr)

# ------- create functions: (choose max gini, not min gini na ja)
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

# for numeric and Date independent var: (Date origin is '1970-01-01')
all_gini_num = function(df, target_col_name, features_col_range) {
  # num.gini = data.frame(feature.name = character(),
  #                       threshold = numeric(),
  #                       gini.score = numeric(),
  #                       stringsAsFactors = F)
  num.gini = data.frame()
  for(i in features_col_range){
    pt = proc.time()
    if(is.numeric(df[[i]])){
      print(paste0('number of NA in ', names(df)[i], ' = ', sum(is.na(df[i]))))
      all.threshold = unique(df[[i]])
      score = sapply(all.threshold, function(x) { gini_index(df[[target_col_name]], df[[i]] < x ) })
      tmp = data.frame(feature.name = rep(names(df)[i], length(all.threshold)),
                       threshold.num = all.threshold,
                       threshold.date = as.Date(NA),
                       gini.score = score,
                       stringsAsFactors = F)
      num.gini = rbind(num.gini, tmp)
    } else if(class(df[[i]]) == 'Date') {
      print(paste0('number of NA in ', names(df)[i], ' = ', sum(is.na(df[i]))))
      all.threshold = unique(df[[i]])
      score = sapply(all.threshold, function(x) { gini_index(df[[target_col_name]], as.numeric(df[[i]]) < as.numeric(x) ) })
      tmp = data.frame(feature.name = rep(names(df)[i], length(all.threshold)),
                       threshold.date = all.threshold,
                       threshold.num = as.numeric(NA),
                       gini.score = score,
                       stringsAsFactors = F)
      num.gini = rbind(num.gini, tmp)
    } else {
      stop(paste0(names(df)[i],' is not numeric or Date'))
    }
    print(proc.time() - pt)
  }
  return(num.gini)
}

# for categorical independent var:
all_gini_cat = function(df, target_col_num, features_col_range){
  cat.gini = data.frame()
  for(i in features_col_range){
    if(is.character(df[[i]])){
      print(paste0('processing ', names(df)[i], ', NA = ', sum(is.na(df[i]))))
      values = unique(df[[i]])
      for(group.num in 1:(length(values)/2)){
        group1.num = group.num
        group2.num = length(values) - group1.num
        group1.prob = as.data.frame(combn(values, group1.num))
        for(group1.case in 1:ncol(group1.prob)){
          gini.case = gini_index(df[[target_col_num]], df[[i]] %in% group1.prob[[group1.case]])
          tmp = data.frame(feature.name = names(df)[i],
                           threshold.cat.g1 = paste(group1.prob[[group1.case]], collapse = ', '),
                           threshold.cat.g2 = paste(values[!(values %in% group1.prob[[group1.case]])], collapse = ', '),
                           gini = gini.case,
                           stringsAsFactors = F)
          cat.gini = rbind(cat.gini, tmp)
        }
      }
    } else {
      stop(paste0(names(df)[i],' is not character'))
    }
  }
  return(cat.gini)
}


############################################# END #######################################



#------------------ test : import
#setwd('C:\\My Jobs\\gini')
setwd('D:\\R_codes\\Mortgage')
df = read.csv("data_banknote_authentication.txt", header = FALSE) #df$V5 is the target variable

# make fake data:
df$V4.1 = sample(c('Y','N','O','P','U','U','Y'), 1372, replace = T)
df$V4.2 = sample(c('Y','N','N'), 1372, replace = T)
v5 = df$V5
df$V5 = NULL
df$V5 = v5

# TEST my functions : ######## HOW TO USE THE FUNCTIONS ######
df2 = all_gini_num(df, 'V5', 1:5)
df3 = all_gini_cat(df, 'V5', 5:6)

df3 %>% 
  group_by(feature.name) %>% 
  mutate(the.rank = rank(-gini.score, ties.method = 'random')) %>%  
  filter(the.rank == 1) %>% 
  select(-the.rank)


#-------How to use gini_index function:

# calculate gini for threshold = 1.7:
gini_index(df$V5, df$V1 < 1.7)
# find max gini of all threshold


#-------- build tree from rpart:
library(rpart)
library(rpart.plot)
mod1 = rpart(V5 ~ V4.1 + V4.2, data = df, method ='class', cp = 0.001)
prp(mod1)
