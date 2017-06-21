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


View(dfclosed[dfclosed$ACCOUNT_TYPE == '22', 
              c('Loan_amount1', 'Amount_owe1', 
                'Loan_amount2', 'Amount_owe2', 
                'Loan_amount3', 'Amount_owe3', 
                'Loan_amount4', 'Amount_owe4', 
                'Loan_amount5', 'Amount_owe5', 
                'Loan_amount6', 'Amount_owe6', 
                'Delay_Payment_code1', 
                'Delay_Payment_code2', 
                'Delay_Payment_code3', 
                'Delay_Payment_code4', 
                'Delay_Payment_code5', 
                'Delay_Payment_code6',
                'DATE_ACCOUNT_OPENED',
                'DATE_ACCOUNT_CLOSED')])

View(dfopen[dfopen$ACCOUNT_TYPE == '22' &
              dfopen$Amount_owe5 == 0 &
              !is.na(dfopen$Amount_owe5), 
              c('Loan_amount1', 'Amount_owe1', 
                'Loan_amount2', 'Amount_owe2', 
                'Loan_amount3', 'Amount_owe3', 
                'Loan_amount4', 'Amount_owe4', 
                'Loan_amount5', 'Amount_owe5', 
                'Loan_amount6', 'Amount_owe6', 
                'Delay_Payment_code1', 
                'Delay_Payment_code2', 
                'Delay_Payment_code3', 
                'Delay_Payment_code4', 
                'Delay_Payment_code5', 
                'Delay_Payment_code6',
                'DATE_ACCOUNT_OPENED',
                'DATE_ACCOUNT_CLOSED')])
