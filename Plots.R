# Histogram : TEST

ggplot(data = q1, aes(x=Momentum1)) + 
  geom_histogram(binwidth = 1, center = 0) + 
  scale_x_continuous(breaks = -10:10) +
  stat_bin(binwidth = 1, geom = 'text', aes(label = ..count..), center = 0, vjust = -0.5, size = 3, color = 'red')


################################################################


# Util1
ggplot(data = df[df$Utilization1 < 3.5 & !is.na(df$Utilization1),], aes(x=Utilization1)) + 
  geom_histogram(binwidth = 0.1, aes(fill=Ever30plus_n12MTH)) + 
  scale_x_continuous(breaks=seq(-0.2,3.5,0.1)) +
  stat_bin(binwidth = 0.1 ,geom = 'text', aes(label = ..count.. , fill = Ever30plus_n12MTH), vjust = -0.5, size = 3, color = 'red')
