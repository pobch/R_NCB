tblFun <- function(x){
  tbl <- table(x, useNA = 'always')
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  res
}

tblFun2 <- function(x, y){
  tbl <- table(x, y, useNA = 'always')
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  # colnames(res) <- c('Count','Percentage')
  res
}

# do.call(rbind,lapply(df$ACCOUNT_TYPE,tblFun))
tblFun(df$ACCOUNT_TYPE)

interval = cut(df$Utilization1, breaks = c(seq(-0.01,1.5,0.01), seq(1.6,5,0.1), 1000000000))
percentY = as.data.frame.matrix(table(interval, df$Ever30plus_n12MTH, useNA = 'always'))
rownames(percentY)[is.na(rownames(percentY))] = 'NA'
colnames(percentY)[is.na(colnames(percentY))] = 'NA'
percentY$percentY = round(percentY$Y * 100 / (percentY$N + percentY$Y), 2)
percentY
