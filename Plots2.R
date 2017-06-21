
#### EXPLORE CLOSED ACCOUNT (ALL ACCOUNT TYPE)
closed = df[df$DATE_ACCOUNT_CLOSED < "2015-08-01" & df$DATE_ACCOUNT_CLOSED > "1900-01-30" , 
              c('Delay_Payment_code1',
                'Delay_Payment_code2',
                'Delay_Payment_code3',
                'Delay_Payment_code4',
                'Delay_Payment_code5',
                'Delay_Payment_code6',
                'DATE_ACCOUNT_CLOSED')]

closed = df[df$DATE_ACCOUNT_CLOSED < "1900-01-30" & df$ACCOUNT_STATUS == 11, ]
write.csv(closed, 'closed.csv')

####### END


ggplot(data = plrevo, aes(x = Ever60plus_6MTH, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill')


ggplot(data = plrevo, aes(x = MOB1, fill = Ever30plus_n12MTH)) +
  geom_histogram(position = 'fill')

ggplot(data = plnonrevo, aes(x = Min_Momentum, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill')

ggplot(data = plnonrevo, aes(x = Max_Momentum, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill')

ggplot(data = plnonrevo, aes(x = nactiveloans, fill = Ever30plus_n12MTH)) +
  geom_histogram(position = 'fill', binwidth = 1) +
  scale_x_continuous(limits = c(-1, 75))

ggplot(data = plrevo, aes(x = Amount_Owe_Momentum1, fill = Ever30plus_n12MTH)) +
  geom_histogram() +
  scale_x_continuous(limits = c(-500000, 500000))



