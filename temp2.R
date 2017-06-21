ccdf$maxowe = apply(ccdf[, c('Amount_owe1', 'Amount_owe2', 'Amount_owe3',
                             'Amount_owe4', 'Amount_owe5', 'Amount_owe6')],
                    1,
                    max)
ccdf$maxloan = apply(ccdf[, c('Loan_amount1', 'Loan_amount2', 'Loan_amount3',
                              'Loan_amount4', 'Loan_amount5', 'Loan_amount6')],
                     1,
                     max)

ggplot(data = ccdf, aes(x = maxowe)) +
  geom_histogram(binwidth = 500) +
  scale_x_continuous(limits = c(-500,20000))

sort(table(ccdf$maxowe), decreasing = T)
sort(table(ccdf$maxloan), decreasing = T)



ggplot(data = ccdf, aes(x = nloans, fill = Ever30plus_n12MTH)) +
  geom_histogram(position = 'dodge', binwidth = 1) +
  scale_x_continuous(breaks = seq(0,100,1))



ggplot(data = ccdf, aes(x=stdUtil123, fill = Ever30plus_n12MTH)) +
  geom_histogram(binwidth = 0.01) +
  scale_x_continuous(limits = c(-0.01,1.05))

table(ccdf$stdUtil1to6)
