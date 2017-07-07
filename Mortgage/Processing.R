library(ggplot2)

ggplot(data = subdf, aes(x = MINIMUM_PERCENT_PAYMENT)) +
  geom_histogram(binwidth = 1)

ggplot(data = subdf, aes(x = Delay_Payment_code2)) +
  geom_bar(stat = 'count')
