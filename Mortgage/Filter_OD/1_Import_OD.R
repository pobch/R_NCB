setwd('D:\\SAS_data\\Chk_data')
library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(caTools)
library(ROCR)
library(ROSE)

df = read_csv("MG_ALL_18MTH.csv", 
              col_types = cols(
                SYSTEM_ID = col_character(),
                TRADE_SERIAL = col_character(),
                INSTALLMENT_FREQUENCY = col_character(),
                ACCOUNT_STATUS = col_character(),
                DELAY_PAYMENT_CODE = col_character(),
                Delay_Payment_code_n1 = col_character(),
                Delay_Payment_code_n2 = col_character(),
                Delay_Payment_code_n3 = col_character(),
                Delay_Payment_code_n4 = col_character(),
                Delay_Payment_code_n5 = col_character(),
                Delay_Payment_code_n6 = col_character(),
                Delay_Payment_code_n7 = col_character(),
                Delay_Payment_code_n8 = col_character(),
                Delay_Payment_code_n9 = col_character(),
                Delay_Payment_code_n10 = col_character(),
                Delay_Payment_code_n11 = col_character(),
                All_Payment_code_n = col_character(),
                OWNERSHIP_INDICATOR = col_character(),
                DATE_ACCOUNT_CLOSED = col_date(format = '%d%b%Y'),
                REPORT_DATE = col_date(format = '%d%b%Y'),
                DATE_ACCOUNT_OPENED = col_date(format = '%d%b%Y'),
                DATE_LAST_PAYMENT = col_date(format = '%d%b%Y'),
                DEFAULT_DATE = col_date(format = '%d%b%Y'),
                DATE_LAST_RESTRUCTURED = col_date(format = '%d%b%Y')
              ))

glimpse(df)


# reorder columns
df = df[, c("SYSTEM_ID", "TRADE_SERIAL", "ID", "CUSTOMER_TYPE", "OWNERSHIP_INDICATOR", 
            "REPORT_DATE", "DATE_ACCOUNT_OPENED", "MOB1", "DATE_ACCOUNT_CLOSED",
            "DATE_LAST_PAYMENT",  "DEFAULT_DATE", "DATE_LAST_RESTRUCTURED", 
            "INSTALLMENT_FREQUENCY", "INSTALLMENT_NUMBER_PAYMENT", "INSTALLMENT_AMOUNT", "AMOUNT_PAST_DUE",
            "ACCOUNT_TYPE",   
            "ACCOUNT_STATUS", "TYPE_OF_CREDIT_CARD", "MINIMUM_PERCENT_PAYMENT",
            "Delay_Payment_code1", "Delay_Payment_code2", "Delay_Payment_code3", "Delay_Payment_code4",
            "Delay_Payment_code5", "Delay_Payment_code6", "All_Payment_code",
            "Loan_amount1", "Loan_amount2", "Loan_amount3", "Loan_amount4", "Loan_amount5","Loan_amount6", 
            "Amount_owe1", "Amount_owe2", "Amount_owe3", "Amount_owe4", "Amount_owe5", "Amount_owe6",  
            "Utilization1", "Utilization2", "Utilization3", "Utilization4", 
            "Utilization5", "Utilization6", "AVG_Utilization_3MTH", "AVG_Utilization_6MTH", 
            "Amount_Owe_Momentum1", "Amount_Owe_Momentum2", "Amount_Owe_Momentum3", 
            "Amount_Owe_Momentum4", "Amount_Owe_Momentum5", "Utilization_Momentum1", 
            "Utilization_Momentum2", "Utilization_Momentum3", "Utilization_Momentum4", 
            "Utilization_Momentum5",  "All_0_6Mth", "ALL_0_below6Mth", 
            "Ever30plus_6MTH", "Ever30plus_5MTH", "Ever30plus_4MTH", "Ever30plus_3MTH", 
            "Ever60plus_6MTH", "Ever60plus_5MTH", "Ever60plus_4MTH", "Ever60plus_3MTH", 
            "EverXplus_6MTH", "EverXplus_5MTH", "EverXplus_4MTH", "EverXplus_3MTH", 
            "Momentum1", "Momentum2", "Momentum3", "Momentum4", "Momentum5", 
            "Min_Momentum", "Max_Momentum", "Number_of_1", "Number_of_1plus", 
            "Number_of_2", "Number_of_2plus", "Number_of_3plus", "ACCOUNT_STATUS_CODE", 
            "Number_OF_Term", "Num_OF_Y", "Num_OF_N", "Count_Y", "Momentum1_OD", 
            "Momentum2_OD", "Momentum3_OD", "Momentum4_OD", "Momentum5_OD", 
            "Min_Momentum_OD", "Max_Momentum_OD", "Num_PL", "Num_CC", "Num_HP", 
            "Num_HL", "Num_OD", "Num_OTH_HP", "Num_CL", "Num_COL", "Num_RD", 
            "Num_OL", "Num_A_PL", "Num_A_CC", "Num_A_HP", "Num_A_HL", "Num_A_OD", 
            "Num_A_OTH_HP", "Num_A_CL", "Num_A_COL", "Num_A_RD", "Num_A_OL", 
            "Loan_amount", "Amount_owe", "DELAY_PAYMENT_CODE", "Utilization", 
            "Loan_amount_n1", "Amount_owe_n1", "Delay_Payment_code_n1", "Utilization_n1", 
            "Loan_amount_n2", "Amount_owe_n2", "Delay_Payment_code_n2", "Utilization_n2", 
            "Loan_amount_n3", "Amount_owe_n3", "Delay_Payment_code_n3", "Utilization_n3", 
            "Loan_amount_n4", "Amount_owe_n4", "Delay_Payment_code_n4", "Utilization_n4", 
            "Loan_amount_n5", "Amount_owe_n5", "Delay_Payment_code_n5", "Utilization_n5", 
            "Loan_amount_n6", "Amount_owe_n6", "Delay_Payment_code_n6", "Utilization_n6", 
            "Loan_amount_n7", "Amount_owe_n7", "Delay_Payment_code_n7", "Utilization_n7", 
            "Loan_amount_n8", "Amount_owe_n8", "Delay_Payment_code_n8", "Utilization_n8", 
            "Loan_amount_n9", "Amount_owe_n9", "Delay_Payment_code_n9", "Utilization_n9", 
            "Loan_amount_n10", "Amount_owe_n10", "Delay_Payment_code_n10", 
            "Utilization_n10", "Loan_amount_n11", "Amount_owe_n11", "Delay_Payment_code_n11", 
            "Utilization_n11", "All_Payment_code_n", "AVG_Utilization_n3MTH", 
            "AVG_Utilization_n6MTH", "AVG_Utilization_n9MTH", "AVG_Utilization_n12MTH", 
            "Amount_Owe_Momentum_n1", "Amount_Owe_Momentum_n2", "Amount_Owe_Momentum_n3", 
            "Amount_Owe_Momentum_n4", "Amount_Owe_Momentum_n5", "Amount_Owe_Momentum_n6", 
            "Amount_Owe_Momentum_n7", "Amount_Owe_Momentum_n8", "Amount_Owe_Momentum_n9", 
            "Amount_Owe_Momentum_n10", "Amount_Owe_Momentum_n11", "Utilization_Momentum_n1", 
            "Utilization_Momentum_n2", "Utilization_Momentum_n3", "Utilization_Momentum_n4", 
            "Utilization_Momentum_n5", "Utilization_Momentum_n6", "Utilization_Momentum_n7", 
            "Utilization_Momentum_n8", "Utilization_Momentum_n9", "Utilization_Momentum_n10", 
            "Utilization_Momentum_n11", "ALL_0_n12MTh", "ALL_0_below_n12Mth", 
            "EverXplus_n12MTH", "Ever30plus_n12MTH", "Ever60plus_n12MTH", 
            "Number_of_1_n12MTH", "Number_of_1plus_n12MTH", "Number_of_2_n12MTH", 
            "Number_of_2plus_n12MTH", "Number_of_3plus_n12MTH", "max_delay", 
            "Momentum_n1", "Momentum_n2", "Momentum_n3", "Momentum_n4", "Momentum_n5", 
            "Momentum_n6", "Momentum_n7", "Momentum_n8", "Momentum_n9", "Momentum_n10", 
            "Momentum_n11", "Min_Momentum_n", "Max_Momentum_n")]

# ----------------- DELETED : 

# df = subset(df, !is.na(Loan_amount1) &
#                !is.na(Loan_amount2) &
#                !is.na(Loan_amount3) &
#                !is.na(Loan_amount4) &
#                !is.na(Loan_amount5) &
#                !is.na(Loan_amount6) &
#                !is.na(Amount_owe1) &
#                !is.na(Amount_owe2) &
#                !is.na(Amount_owe3) &
#                !is.na(Amount_owe4) &
#                !is.na(Amount_owe5) &
#                !is.na(Amount_owe6))

#---------------------------------------------

# change type :
for(i in c("Delay_Payment_code1", "Delay_Payment_code2", "Delay_Payment_code3", "Delay_Payment_code4", "Delay_Payment_code5", "Delay_Payment_code6")){
  df[[i]] = ifelse(df[[i]] == 'F', '10', 
                   ifelse(df[[i]] == 'Y', '1',
                          ifelse(df[[i]] == 'N', '0', df[[i]])))
  df[[i]] = as.numeric(df[[i]])
}


# add new vars
df$nUtil123 = apply(df[, c('Utilization1', 'Utilization2', 'Utilization3')], 1, function(x) {sum(!is.na(x))}) 
df$stdUtil123 = apply(df[ , c('Utilization1', 'Utilization2', 'Utilization3')], 1, function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)} )

df$nUtil456 = apply(df[, c('Utilization4', 'Utilization5', 'Utilization6')], 1, function(x) {sum(!is.na(x))})
df$stdUtil456 = apply(df[ , c('Utilization4', 'Utilization5', 'Utilization6')], 1, function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)})

df$nUtil1to6 = apply(df[, c('Utilization1', 'Utilization2', 'Utilization3', 'Utilization4', 'Utilization5', 'Utilization6')], 1, function(x) {sum(!is.na(x))})
df$stdUtil1to6 = apply(df[ , c('Utilization1', 'Utilization2', 'Utilization3', 'Utilization4', 'Utilization5', 'Utilization6')], 1, function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)})

df$nloans = apply(df[, 96:105], 1, sum)
df$nactiveloans = apply(df[, 106:115], 1, sum)

# Check
table(df$nUtil1to6 == (df$nUtil123 + df$nUtil456))

# add new vars
df$max_util_momentum = apply(df[, c('Utilization_Momentum1', 'Utilization_Momentum2', 'Utilization_Momentum3',
                                    'Utilization_Momentum4', 'Utilization_Momentum5')], 1,
                             max, na.rm = T)

df$min_util_momentum = apply(df[, c('Utilization_Momentum1', 'Utilization_Momentum2', 'Utilization_Momentum3',
                                    'Utilization_Momentum4', 'Utilization_Momentum5')], 1,
                             min, na.rm = T)


#----------------------- count number of NA in each column:
sapply(df, function(x) {sum(is.na(x))})
# or
colSums(is.na(df))


#------------------------ Replace missing value (Pob's version):
df$INSTALLMENT_FREQUENCY = ifelse(is.na(df$INSTALLMENT_FREQUENCY) & df$ACCOUNT_TYPE == '04',
                                  '0',
                                  ifelse(is.na(df$INSTALLMENT_FREQUENCY) & df$ACCOUNT_TYPE == '22',
                                         '3',
                                         df$INSTALLMENT_FREQUENCY)
                                  )
df$TYPE_OF_CREDIT_CARD = ifelse(is.na(df$TYPE_OF_CREDIT_CARD),
                                '0',
                                df$TYPE_OF_CREDIT_CARD)
df$MINIMUM_PERCENT_PAYMENT = ifelse(is.na(df$MINIMUM_PERCENT_PAYMENT),
                                    0,
                                    df$MINIMUM_PERCENT_PAYMENT)
df$is.DeLoOw2.NA = ifelse(is.na(df$Delay_Payment_code2),
                          'Yes',
                          'No') 
df$is.DeLoOw3.NA = ifelse(is.na(df$Delay_Payment_code3),
                          'Yes',
                          'No') 
df$is.DeLoOw4.NA = ifelse(is.na(df$Delay_Payment_code4),
                          'Yes',
                          'No') 
df$is.DeLoOw5.NA = ifelse(is.na(df$Delay_Payment_code5),
                          'Yes',
                          'No') 
df$is.DeLoOw6.NA = ifelse(is.na(df$Delay_Payment_code6),
                          'Yes',
                          'No')   

df$Delay_Payment_code2 = ifelse(is.na(df$Delay_Payment_code2),
                                0,
                                df$Delay_Payment_code2)
df$Delay_Payment_code3 = ifelse(is.na(df$Delay_Payment_code3),
                                0,
                                df$Delay_Payment_code3)
df$Delay_Payment_code4 = ifelse(is.na(df$Delay_Payment_code4),
                                0,
                                df$Delay_Payment_code4)
df$Delay_Payment_code5 = ifelse(is.na(df$Delay_Payment_code5),
                                0,
                                df$Delay_Payment_code5)
df$Delay_Payment_code6 = ifelse(is.na(df$Delay_Payment_code6),
                                0,
                                df$Delay_Payment_code6)
df$Loan_amount2 = ifelse(is.na(df$Loan_amount2),
                         0,
                         df$Loan_amount2)
df$Loan_amount3 = ifelse(is.na(df$Loan_amount3),
                         0,
                         df$Loan_amount3)
df$Loan_amount4 = ifelse(is.na(df$Loan_amount4),
                         0,
                         df$Loan_amount4)
df$Loan_amount5 = ifelse(is.na(df$Loan_amount5),
                         0,
                         df$Loan_amount5)
df$Loan_amount6 = ifelse(is.na(df$Loan_amount6),
                         0,
                         df$Loan_amount6)
df$Amount_owe2 = ifelse(is.na(df$Amount_owe2),
                        0,
                        df$Amount_owe2)
df$Amount_owe3 = ifelse(is.na(df$Amount_owe3),
                        0,
                        df$Amount_owe3)
df$Amount_owe4 = ifelse(is.na(df$Amount_owe4),
                        0,
                        df$Amount_owe4)
df$Amount_owe5 = ifelse(is.na(df$Amount_owe5),
                        0,
                        df$Amount_owe5)
df$Amount_owe6 = ifelse(is.na(df$Amount_owe6),
                        0,
                        df$Amount_owe6)

df$AVG_Utilization_3MTH = ifelse(is.na(df$AVG_Utilization_3MTH),
                                 0,
                                 df$AVG_Utilization_3MTH)
df$AVG_Utilization_6MTH = ifelse(is.na(df$AVG_Utilization_6MTH),
                                 0,
                                 df$AVG_Utilization_6MTH)
df$is.Util1.NA = ifelse(is.na(df$Utilization1),
                        'Yes',
                        'No')
df$is.Util2.NA = ifelse(is.na(df$Utilization2),
                        'Yes',
                        'No')
df$is.Util3.NA = ifelse(is.na(df$Utilization3),
                        'Yes',
                        'No')
df$is.Util4.NA = ifelse(is.na(df$Utilization4),
                        'Yes',
                        'No')
df$is.Util5.NA = ifelse(is.na(df$Utilization5),
                        'Yes',
                        'No')
df$is.Util6.NA = ifelse(is.na(df$Utilization6),
                        'Yes',
                        'No')

df$Utilization1 = ifelse(is.na(df$Utilization1),
                         0,
                         df$Utilization1)
df$Utilization2 = ifelse(is.na(df$Utilization2),
                         0,
                         df$Utilization2)
df$Utilization3 = ifelse(is.na(df$Utilization3),
                         0,
                         df$Utilization3)
df$Utilization4 = ifelse(is.na(df$Utilization4),
                         0,
                         df$Utilization4)
df$Utilization5 = ifelse(is.na(df$Utilization5),
                         0,
                         df$Utilization5)
df$Utilization6 = ifelse(is.na(df$Utilization6),
                         0,
                         df$Utilization6)
df$is.OweMomen1.NA = ifelse(is.na(df$Amount_Owe_Momentum1),
                            'Yes',
                            'No')
df$is.OweMomen2.NA = ifelse(is.na(df$Amount_Owe_Momentum2),
                            'Yes',
                            'No')
df$is.OweMomen3.NA = ifelse(is.na(df$Amount_Owe_Momentum3),
                            'Yes',
                            'No')
df$is.OweMomen4.NA = ifelse(is.na(df$Amount_Owe_Momentum4),
                            'Yes',
                            'No')
df$is.OweMomen5.NA = ifelse(is.na(df$Amount_Owe_Momentum5),
                            'Yes',
                            'No')
df$Amount_Owe_Momentum1 = ifelse(is.na(df$Amount_Owe_Momentum1),
                                 0,
                                 df$Amount_Owe_Momentum1)
df$Amount_Owe_Momentum2 = ifelse(is.na(df$Amount_Owe_Momentum2),
                                 0,
                                 df$Amount_Owe_Momentum2)
df$Amount_Owe_Momentum3 = ifelse(is.na(df$Amount_Owe_Momentum3),
                                 0,
                                 df$Amount_Owe_Momentum3)
df$Amount_Owe_Momentum4 = ifelse(is.na(df$Amount_Owe_Momentum4),
                                 0,
                                 df$Amount_Owe_Momentum4)
df$Amount_Owe_Momentum5 = ifelse(is.na(df$Amount_Owe_Momentum5),
                                 0,
                                 df$Amount_Owe_Momentum5)
df$is.UtilMomen1.NA = ifelse(is.na(df$Utilization_Momentum1),
                             'Yes',
                             'No')
df$is.UtilMomen2.NA = ifelse(is.na(df$Utilization_Momentum2),
                             'Yes',
                             'No')
df$is.UtilMomen3.NA = ifelse(is.na(df$Utilization_Momentum3),
                             'Yes',
                             'No')
df$is.UtilMomen4.NA = ifelse(is.na(df$Utilization_Momentum4),
                             'Yes',
                             'No')
df$is.UtilMomen5.NA = ifelse(is.na(df$Utilization_Momentum5),
                             'Yes',
                             'No')
df$Utilization_Momentum1 = ifelse(is.na(df$Utilization_Momentum1),
                                  0,
                                  df$Utilization_Momentum1)
df$Utilization_Momentum2 = ifelse(is.na(df$Utilization_Momentum2),
                                  0,
                                  df$Utilization_Momentum2)
df$Utilization_Momentum3 = ifelse(is.na(df$Utilization_Momentum3),
                                  0,
                                  df$Utilization_Momentum3)
df$Utilization_Momentum4 = ifelse(is.na(df$Utilization_Momentum4),
                                  0,
                                  df$Utilization_Momentum4)
df$Utilization_Momentum5 = ifelse(is.na(df$Utilization_Momentum5),
                                  0,
                                  df$Utilization_Momentum5)
df$is.Momentum2.NA = ifelse(is.na(df$Momentum2),
                            'Yes',
                            'No')
df$is.Momentum3.NA = ifelse(is.na(df$Momentum3),
                            'Yes',
                            'No')
df$is.Momentum4.NA = ifelse(is.na(df$Momentum4),
                            'Yes',
                            'No')
df$is.Momentum5.NA = ifelse(is.na(df$Momentum5),
                            'Yes',
                            'No')
df$Momentum2 = ifelse(is.na(df$Momentum2),
                      0,
                      df$Momentum2)
df$Momentum3 = ifelse(is.na(df$Momentum3),
                      0,
                      df$Momentum3)
df$Momentum4 = ifelse(is.na(df$Momentum4),
                      0,
                      df$Momentum4)
df$Momentum5 = ifelse(is.na(df$Momentum5),
                      0,
                      df$Momentum5)
df$is.stdUtil123.NA = ifelse(is.na(df$stdUtil123),
                             'Yes',
                             'No')
df$is.stdUtil456.NA = ifelse(is.na(df$stdUtil456),
                             'Yes',
                             'No')
df$is.stdUtil1to6.NA = ifelse(is.na(df$stdUtil1to6),
                              'Yes',
                              'No')
df$stdUtil123 = ifelse(is.na(df$stdUtil123),
                       0,
                       df$stdUtil123)
df$stdUtil456 = ifelse(is.na(df$stdUtil456),
                       0,
                       df$stdUtil456)
df$stdUtil1to6 = ifelse(is.na(df$stdUtil1to6),
                        0,
                        df$stdUtil1to6)

#---------------------------------------------------------


# list all column names:
dput(names(df))

# choose columns for OD and not-OD:
subdf = df[which(df$ACCOUNT_TYPE != '04'), c(
            "All_0_6Mth", "ALL_0_below6Mth", 
            "Ever30plus_6MTH", "Ever30plus_5MTH", "Ever30plus_4MTH", "Ever30plus_3MTH", 
            "Ever60plus_6MTH", "Ever60plus_5MTH", "Ever60plus_4MTH", "Ever60plus_3MTH", 
            "EverXplus_6MTH", "EverXplus_5MTH", "EverXplus_4MTH", "EverXplus_3MTH", 
            "Momentum1", "Momentum2", "Momentum3", "Momentum4", "Momentum5", 
            "Min_Momentum", "Max_Momentum", 
            "Number_of_1", "Number_of_1plus", 
            "Number_of_2", "Number_of_2plus", 
            "Number_of_3plus", 
            
            "SYSTEM_ID", "TRADE_SERIAL",  
            "CUSTOMER_TYPE", "OWNERSHIP_INDICATOR", 
            "REPORT_DATE", "DATE_ACCOUNT_OPENED", "DATE_ACCOUNT_CLOSED",
            "DATE_LAST_PAYMENT",  "DEFAULT_DATE", "DATE_LAST_RESTRUCTURED", 
            "MOB1", 
            "INSTALLMENT_FREQUENCY", "INSTALLMENT_NUMBER_PAYMENT", "INSTALLMENT_AMOUNT", 
            "MINIMUM_PERCENT_PAYMENT", "AMOUNT_PAST_DUE",
            "ACCOUNT_TYPE", "TYPE_OF_CREDIT_CARD", "ACCOUNT_STATUS", "ACCOUNT_STATUS_CODE",
            "Delay_Payment_code1", "Delay_Payment_code2", "Delay_Payment_code3", 
            "Delay_Payment_code4", "Delay_Payment_code5", "Delay_Payment_code6", 
            "All_Payment_code",
            "Loan_amount1", "Loan_amount2", "Loan_amount3", 
            "Loan_amount4", "Loan_amount5", "Loan_amount6", 
            "Amount_owe1", "Amount_owe2", "Amount_owe3", 
            "Amount_owe4", "Amount_owe5", "Amount_owe6",  
            "Utilization1", "Utilization2", "Utilization3", 
            "Utilization4", "Utilization5", "Utilization6", 
            "AVG_Utilization_3MTH", "AVG_Utilization_6MTH", 
            "Amount_Owe_Momentum1", "Amount_Owe_Momentum2", "Amount_Owe_Momentum3", 
            "Amount_Owe_Momentum4", "Amount_Owe_Momentum5", 
            "Utilization_Momentum1", "Utilization_Momentum2", "Utilization_Momentum3", 
            "Utilization_Momentum4", "Utilization_Momentum5", 
            "Num_PL", "Num_CC", "Num_HP", "Num_HL", "Num_OD", "Num_OTH_HP", 
            "Num_CL", "Num_COL", "Num_RD", "Num_OL", 
            "Num_A_PL", "Num_A_CC", "Num_A_HP", "Num_A_HL", "Num_A_OD", 
            "Num_A_OTH_HP", "Num_A_CL", "Num_A_COL", "Num_A_RD", "Num_A_OL", 
            "nloans", "nactiveloans", 
            "nUtil123", "nUtil456", "nUtil1to6",
            "stdUtil123", "stdUtil456", "stdUtil1to6", 
            "max_util_momentum", "min_util_momentum", 
            
            "is.DeLoOw2.NA", "is.DeLoOw3.NA", "is.DeLoOw4.NA", "is.DeLoOw5.NA", 
            "is.DeLoOw6.NA", "is.Util1.NA", "is.Util2.NA", "is.Util3.NA", 
            "is.Util4.NA", "is.Util5.NA", "is.Util6.NA", "is.OweMomen1.NA", 
            "is.OweMomen2.NA", "is.OweMomen3.NA", "is.OweMomen4.NA", "is.OweMomen5.NA", 
            "is.UtilMomen1.NA", "is.UtilMomen2.NA", "is.UtilMomen3.NA", "is.UtilMomen4.NA", 
            "is.UtilMomen5.NA", "is.Momentum2.NA", "is.Momentum3.NA", "is.Momentum4.NA", 
            "is.Momentum5.NA", "is.stdUtil123.NA", "is.stdUtil456.NA", "is.stdUtil1to6.NA",
            
            "Ever30plus_n12MTH")]

subdf_od = df[which(df$ACCOUNT_TYPE == '04'), c(
            "Number_OF_Term", "Num_OF_Y", "Num_OF_N", "Count_Y", 
            "Momentum1_OD", "Momentum2_OD", "Momentum3_OD", "Momentum4_OD", "Momentum5_OD", 
            "Min_Momentum_OD", "Max_Momentum_OD",
            
            "SYSTEM_ID", "TRADE_SERIAL",  
            "CUSTOMER_TYPE", "OWNERSHIP_INDICATOR", 
            "REPORT_DATE", "DATE_ACCOUNT_OPENED", "DATE_ACCOUNT_CLOSED",
            "DATE_LAST_PAYMENT",  "DEFAULT_DATE", "DATE_LAST_RESTRUCTURED", 
            "MOB1", 
            "INSTALLMENT_FREQUENCY", "INSTALLMENT_NUMBER_PAYMENT", "INSTALLMENT_AMOUNT", 
            "MINIMUM_PERCENT_PAYMENT", "AMOUNT_PAST_DUE",
            "ACCOUNT_TYPE", "TYPE_OF_CREDIT_CARD", "ACCOUNT_STATUS", "ACCOUNT_STATUS_CODE",
            "Delay_Payment_code1", "Delay_Payment_code2", "Delay_Payment_code3", 
            "Delay_Payment_code4", "Delay_Payment_code5", "Delay_Payment_code6", 
            "All_Payment_code",
            "Loan_amount1", "Loan_amount2", "Loan_amount3", 
            "Loan_amount4", "Loan_amount5", "Loan_amount6", 
            "Amount_owe1", "Amount_owe2", "Amount_owe3", 
            "Amount_owe4", "Amount_owe5", "Amount_owe6",  
            "Utilization1", "Utilization2", "Utilization3", 
            "Utilization4", "Utilization5", "Utilization6", 
            "AVG_Utilization_3MTH", "AVG_Utilization_6MTH", 
            "Amount_Owe_Momentum1", "Amount_Owe_Momentum2", "Amount_Owe_Momentum3", 
            "Amount_Owe_Momentum4", "Amount_Owe_Momentum5", 
            "Utilization_Momentum1", "Utilization_Momentum2", "Utilization_Momentum3", 
            "Utilization_Momentum4", "Utilization_Momentum5", 
            "Num_PL", "Num_CC", "Num_HP", "Num_HL", "Num_OD", "Num_OTH_HP", 
            "Num_CL", "Num_COL", "Num_RD", "Num_OL", 
            "Num_A_PL", "Num_A_CC", "Num_A_HP", "Num_A_HL", "Num_A_OD", 
            "Num_A_OTH_HP", "Num_A_CL", "Num_A_COL", "Num_A_RD", "Num_A_OL", 
            "nloans", "nactiveloans", 
            "nUtil123", "nUtil456", "nUtil1to6",
            "stdUtil123", "stdUtil456", "stdUtil1to6", 
            "max_util_momentum", "min_util_momentum", 
            
            "is.DeLoOw2.NA", "is.DeLoOw3.NA", "is.DeLoOw4.NA", "is.DeLoOw5.NA", 
            "is.DeLoOw6.NA", "is.Util1.NA", "is.Util2.NA", "is.Util3.NA", 
            "is.Util4.NA", "is.Util5.NA", "is.Util6.NA", "is.OweMomen1.NA", 
            "is.OweMomen2.NA", "is.OweMomen3.NA", "is.OweMomen4.NA", "is.OweMomen5.NA", 
            "is.UtilMomen1.NA", "is.UtilMomen2.NA", "is.UtilMomen3.NA", "is.UtilMomen4.NA", 
            "is.UtilMomen5.NA", "is.Momentum2.NA", "is.Momentum3.NA", "is.Momentum4.NA", 
            "is.Momentum5.NA", "is.stdUtil123.NA", "is.stdUtil456.NA", "is.stdUtil1to6.NA",
            
            "Ever30plus_n12MTH")]

#---------------------- Delete some irrelevant columns: 

df2 = select(subdf_od, -SYSTEM_ID, -TRADE_SERIAL, -CUSTOMER_TYPE, -All_Payment_code)


#---------------------- split train/test for OD loan:
subdf_od = df2 %>% 
  select(-ACCOUNT_TYPE,
         -Num_PL, -Num_HP, -Num_CC, -Num_HL, -Num_OTH_HP, -Num_CL,
         -Num_COL, -Num_RD, -Num_OL, -Num_A_PL, -Num_A_HP, -Num_A_CC, 
         -Num_A_HL, -Num_A_OTH_HP, -Num_A_CL, -Num_A_COL, -Num_A_RD, -Num_A_OL,
         -nloans, -nactiveloans)
subdf_od = subdf_od[-which(subdf_od$ACCOUNT_STATUS == '31'), ] # There are 8 rows that ACC_STATUS == '31'

set.seed(123)
spl = sample.split(subdf_od$Ever30plus_n12MTH, SplitRatio = 0.7)
train_od = subset(subdf_od, spl == T)
test_od = subset(subdf_od, spl == F)

#-----------------------------------
