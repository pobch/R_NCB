# Explore case: at least 1 month that both loan_amount & amount_owe == 0
test = dfclosed[which((dfclosed$Loan_amount1 == 0 & dfclosed$Amount_owe1 == 0) |
           (dfclosed$Loan_amount2 == 0 & dfclosed$Amount_owe2 == 0) |
           (dfclosed$Loan_amount3 == 0 & dfclosed$Amount_owe3 == 0) |
           (dfclosed$Loan_amount4 == 0 & dfclosed$Amount_owe4 == 0) |
           (dfclosed$Loan_amount5 == 0 & dfclosed$Amount_owe5 == 0) |
           (dfclosed$Loan_amount6 == 0 & dfclosed$Amount_owe6 == 0)),]

test2 = dfclosed %>% 
  subset((Loan_amount1 == 0 & Amount_owe1 == 0) |
           (Loan_amount2 == 0 & Amount_owe2 == 0) |
           (Loan_amount3 == 0 & Amount_owe3 == 0) |
           (Loan_amount4 == 0 & Amount_owe4 == 0) |
           (Loan_amount5 == 0 & Amount_owe5 == 0) |
           (Loan_amount6 == 0 & Amount_owe6 == 0))

test3 = dfclosed %>% 
  subset(DATE_ACCOUNT_CLOSED < '2015-08-01' & DATE_ACCOUNT_CLOSED > '1900-02-03')

test4 = dfopen %>% 
  subset((Loan_amount1 == 0 & Amount_owe1 == 0) |
         (Loan_amount2 == 0 & Amount_owe2 == 0) |
         (Loan_amount3 == 0 & Amount_owe3 == 0) |
         (Loan_amount4 == 0 & Amount_owe4 == 0) |
         (Loan_amount5 == 0 & Amount_owe5 == 0) |
         (Loan_amount6 == 0 & Amount_owe6 == 0))
write.csv(test4, 'LA_AO_both0.csv')


test5 = dfopen %>% 
  subset(Loan_amount1 == 0 |
           Loan_amount2 == 0 |
           Loan_amount3 == 0 |
           Loan_amount4 == 0 |
           Loan_amount5 == 0 |
           Loan_amount6 == 0)
write.csv(test5, 'LA_0.csv')


test6 = dfopen %>% 
  subset(DATE_LAST_RESTRUCTURED > '1900-02-15')
write.csv(test6, 'date_last_restruc.csv')

test7 = dfopen %>% 
  subset(ACCOUNT_TYPE == '90')


# ================================================================= #

# choose one of this (same result)
dfA = df %>% 
  subset(ACCOUNT_STATUS_CODE == 'A' & 
           DATE_ACCOUNT_CLOSED < '1900-12-01')
dfA2 = df %>% 
  subset(ACCOUNT_STATUS_CODE == 'A')


dfA3 = dfA2 %>% 
  subset(!is.na(Loan_amount1) &
           !is.na(Loan_amount2) & 
           !is.na(Loan_amount3) &
           !is.na(Loan_amount4) &
           !is.na(Loan_amount5) &
           !is.na(Loan_amount6) &
           !is.na(Amount_owe1) &
           !is.na(Amount_owe2) &
           !is.na(Amount_owe3) &
           !is.na(Amount_owe4) &
           !is.na(Amount_owe5) &
           !is.na(Amount_owe6))

#Explore NA in ALL_0_below6Mth
table(dfA3[which(is.na(dfA3$ALL_0_below6Mth)), "ACCOUNT_TYPE"], useNA = 'always')

#Explore case: loan_amount in all month == 0
View(dfA3[which(dfA3$Loan_amount1 == 0 & 
                  dfA3$Loan_amount2 == 0 & 
                  dfA3$Loan_amount3 == 0 & 
                  dfA3$Loan_amount4 == 0 &
                  dfA3$Loan_amount5 == 0 &
                  dfA3$Loan_amount6 == 0 &
                  dfA3$ACCOUNT_TYPE == '04'), 
          c('ACCOUNT_TYPE', 'Amount_owe1', 'Delay_Payment_code1')])

# ================================================================= #

temp3 = df[df$DATE_LAST_RESTRUCTURED > '1900-05-15', ]



temp4 = (df[df$DEFAULT_DATE > '1900-05-05', c("EverXplus_6MTH", "Ever30plus_6MTH", "Ever60plus_6MTH", "EverXplus_n12MTH", "Ever30plus_n12MTH", "Ever60plus_n12MTH")])



# ==================================================================
gini_process = function(classes, splitvar = NULL){
  if(is.null(splitvar)){
    base_prob = table(classes)/length(classes)
    return(1-sum(base_prob**2))
  }
  base_prob = table(splitvar)/length(splitvar)
  crosstab = table(classes, splitvar)
  crossprob = prop.table(crosstab, 2)
}

