library(combinat)

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
#setwd('C:\\My Jobs\\gini')
setwd('D:\\R_codes\\Mortgage')
df = read.csv("data_banknote_authentication.txt", header = FALSE) #df$V5 is the target variable

#-------THE ANSWER (USE AS CONFIRMATION)---------------------------

gini_index(df$V5, df$V1 < 1.7)

# find best gini index of V1
for(i in 1:nrow(df)){
  df$V1gini[i] = gini_index(df$V5, df$V1 < df$V1[i])
}
which.max(df$V1gini) # ANS:284

#-------PROCESSING-----------------------
all_gini = function(df, target_col_name, features_col_range) {
  for(i in features_col_range){
    print(paste0('number of NA in ', names(df)[i], ' = ', sum(is.na(df[i]))))
    if(is.numeric(df[[i]])){
      df[paste0(names(df)[i], '_gini')] = apply(df, 1, function(x) { gini_index(df[[target_col_name]], df[i] < x[i] ) })
    } else {
      
    }
  }
  return(df)
}

df2 = all_gini(df, 'V5', 1:5)

for(i in 7:11){
  print(max(df2[i], na.rm = T))
}



#---------------------------------------
library(rpart)
mod1 = rpart(V5 ~ V1 + V2 + V3 + V4, df, method ='class')
rpart.plot(mod1)


