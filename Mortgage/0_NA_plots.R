library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)


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
  
  "Ever30plus_n12MTH")]


# plots (trial):
subdf$temp = ifelse(is.na(subdf$MINIMUM_PERCENT_PAYMENT), 'NA', 'Not.NA')
ggplot(data = subdf, aes(x = temp)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  geom_text(aes(y = (..count..)/sum(..count..), label = scales::percent((..count..)/sum(..count..))) , stat = 'count', vjust = -0.5)

ggplot(data = subdf, aes(x = temp)) +
  geom_bar()

# plots:
subdf$temp = ifelse(is.na(subdf$MINIMUM_PERCENT_PAYMENT), 'NA', 'Not.NA')
p1 = ggplot(data = subdf, aes(x = MINIMUM_PERCENT_PAYMENT)) +
  geom_histogram(binwidth = 1) +
  ylim(0, max(table(subdf$MINIMUM_PERCENT_PAYMENT)))
p2 = ggplot(data = subdf[which(subdf$temp == 'NA'),]) +
  geom_bar(aes(x = temp), width = 0.2) +
  ylim(0, max(table(subdf$MINIMUM_PERCENT_PAYMENT))) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  xlab('')
grid.arrange(p1,p2, ncol =2, widths = c(10,0.6))

ggplot(data = subdf, aes(x = Delay_Payment_code2)) +
  geom_bar(stat = 'count')

ggplot(data = subdf, aes(x = Delay_Payment_code6)) +
  geom_bar(stat = 'count')

subdf$temp = cut(subdf$Loan_amount2, breaks = c(-Inf,0,seq(50000, 1000000, by = 50000), Inf))
levels(subdf$temp)[1] = '0'
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) +
  xlab('Loan_amount2')

subdf$temp = cut(subdf$Loan_amount6, breaks = c(-Inf,0,seq(50000, 1000000, by = 50000), Inf))
levels(subdf$temp)[1] = '0'
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) +
  xlab('Loan_amount6')

subdf$temp = cut(subdf$Amount_owe2, breaks = c(-Inf,0,seq(50000, 1000000, by = 50000), Inf))
levels(subdf$temp)[1] = '0'
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) +
  xlab('Amount_owe2')

subdf$temp = cut(subdf$Amount_owe6, breaks = c(-Inf,0,seq(50000, 1000000, by = 50000), Inf))
levels(subdf$temp)[1] = '0'
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) + 
  xlab('Amount_owe6')

subdf$temp = cut(subdf$Utilization1, breaks = c(-Inf, seq(0,1.5,0.02), Inf))
levels(subdf$temp)[1] = '0'
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) +
  xlab('Utilization1')

subdf$temp = cut(subdf$Utilization6, breaks = c(-Inf, seq(0,1.5,0.02), Inf))
levels(subdf$temp)[1] = '0'
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) +
  xlab('Utilization6')

subdf$temp = cut(subdf$AVG_Utilization_3MTH, breaks = c(-Inf, seq(0,1.5,0.02), Inf))
levels(subdf$temp)[1] = '0'
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) + 
  xlab('AVG_Utilization_3MTH')

subdf$temp = cut(subdf$AVG_Utilization_6MTH, breaks = c(-Inf, seq(0,1.5,0.02), Inf))
levels(subdf$temp)[1] = '0'
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) +
  xlab('AVG_Utilization_6MTH')

subdf$temp = cut(subdf$Amount_Owe_Momentum1, breaks = c(-Inf, seq(-30000, -2000, by = 2000), -0.1, seq(0, 30000, by = 2000), Inf))
levels(subdf$temp)[c(16,17)] = c('(-2e+03,0)', '0')
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) + 
  xlab('Amount_Owe_Momentum1')

subdf$temp = cut(subdf$Amount_Owe_Momentum5, breaks = c(-Inf, seq(-30000, -2000, by = 2000), -0.1, seq(0, 30000, by = 2000), Inf))
levels(subdf$temp)[c(16,17)] = c('(-2e+03,0)', '0')
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) + 
  xlab('Amount_Owe_Momentum5')

subdf$temp = cut(subdf$Utilization_Momentum1, breaks = c(-Inf, seq(-1, -0.02, by = 0.02), -0.000001, seq(0, 1, by = 0.02), Inf))
levels(subdf$temp)[c(51,52)] = c('(-0.02,0)','0')
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) +
  xlab('Utilization_Momentum1')

subdf$temp = cut(subdf$Utilization_Momentum5, breaks = c(-Inf, seq(-1, -0.02, by = 0.02), -0.000001, seq(0, 1, by = 0.02), Inf))
levels(subdf$temp)[c(51,52)] = c('(-0.02,0)','0')
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) + 
  xlab('Utilization_Momentum5')

subdf$temp = cut(subdf$stdUtil123, breaks = c(-Inf, seq(0, 3, by = 0.5), Inf))
levels(subdf$temp)[1] = '0'
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) +
  xlab('sd_of_Utilization123')

subdf$temp = cut(subdf$stdUtil456, breaks = c(-Inf, seq(0, 3, by = 0.5), Inf))
levels(subdf$temp)[1] = '0'
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) +
  xlab('sd_of_Utilization456')

subdf$temp = cut(subdf$stdUtil1to6, breaks = c(-Inf, seq(0, 3, by = 0.5), Inf))
levels(subdf$temp)[1] = '0'
ggplot(data = subdf, aes(x = temp)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01)) +
  xlab('sd_of_AllUtilization')

table(subdf$Momentum2)
# -10    -9    -8    -7    -6    -5    -4    -3    -2    -1     0     1     2     3     6     7     8     9    10 
#  11     1     2     4     2     7     4    14    72   270 83574  1853    83     1     3     1     1     1     1 

table(subdf$Momentum5)
# -10    -9    -6    -5    -4    -3    -2    -1     0     1     2     3     7     8    10 
#   3     1     3     4     6    24    69   232 76830  1305    85     3     2     3     3 









