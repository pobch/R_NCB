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


# add new vars
df$nUtil123 = apply(df[, c('Utilization1', 'Utilization2', 'Utilization3')], 1, function(x) {sum(!is.na(x))}) 
df$stdUtil123 = apply(df[ , c('Utilization1', 'Utilization2', 'Utilization3')], 1, sd, na.rm = T)

df$nUtil456 = apply(df[, c('Utilization4', 'Utilization5', 'Utilization6')], 1, function(x) {sum(!is.na(x))})
df$stdUtil456 = apply(df[ , c('Utilization4', 'Utilization5', 'Utilization6')], 1, sd, na.rm = T)

df$nUtil1to6 = apply(df[, c('Utilization1', 'Utilization2', 'Utilization3', 'Utilization4', 'Utilization5', 'Utilization6')], 1, function(x) {sum(!is.na(x))})
df$stdUtil1to6 = apply(df[ , c('Utilization1', 'Utilization2', 'Utilization3', 'Utilization4', 'Utilization5', 'Utilization6')], 1, sd, na.rm = T)

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


# ----------------------------------------------------------------------------------------
df2 = subset(df, !is.na(Loan_amount1) &
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
# ----------------------------------------------------------------------------------------


# subset dataframes

# DEL Closed accounts
dfclosed = df[df$DATE_ACCOUNT_CLOSED > '1900-03-15' | df$ACCOUNT_STATUS_CODE == 'C',]
dfopen = setdiff(df, dfclosed)
# DEL if Loan_amount & Amount_owe == NA at least 1 month
dfopen = subset(dfopen, !is.na(Loan_amount1) &
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

# PL-revo, PL-non-revo, CC, OD
plrevo = dfopen[dfopen$ACCOUNT_TYPE == '05' & dfopen$INSTALLMENT_AMOUNT == 0, ]
plnonrevo = dfopen[dfopen$ACCOUNT_TYPE == '05' & dfopen$INSTALLMENT_AMOUNT > 0,]
ccdf = dfopen[dfopen$ACCOUNT_TYPE == '22', ]
oddf = dfopen[dfopen$ACCOUNT_TYPE == '04', ]
