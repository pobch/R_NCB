setwd('D:\\SAS_data\\Chk_data')

library(readr)

df = read_delim('NIDA_TXT_MTH_1505.csv', delim = '|', 
                col_types = cols(
                  .default = col_character(),
                  SYSTEM_ID = col_integer(),
                  TRADE_SERIAL = col_integer(),
                  OWNERSHIP_INDICATOR = col_character(),
                  INSTALLMENT_FREQUENCY = col_integer(),
                  INSTALLMENT_NUMBER_PAYMENT = col_integer(),
                  CREDIT_LINE = col_number(),
                  CREDIT_USE = col_number(),
                  INSTALLMENT_AMOUNT = col_number(),
                  AMOUNT_PAST_DUE = col_number(),
                  ACCOUNT_STATUS = col_integer(),
                  MINIMUM_PERCENT_PAYMENT = col_integer()
                ))
library(dplyr)
glimpse(df)


df = read_csv("P_loan_future.csv", 
              col_types = cols(
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
                All_Payment_code_n = col_character()
              ))


df = read_csv("P_loan_ALL_18MTH.csv", 
              col_types = cols(
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
                OWNERSHIP_INDICATOR = col_character()
              ))


