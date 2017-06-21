
# 1 dimension: EverX, 30, 60 of 6 MTH

ggplot(data = ccdf, aes(x = EverXplus_6MTH, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill')

ggsave('C:\\My Proj\\EverX.png', width = 10, height = 10, units = 'cm')


ggplot(data = ccdf, aes(x = Ever30plus_6MTH, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill')

ggsave('C:\\My Proj\\Ever30.png', width = 10, height = 10, units = 'cm')


ggplot(data = ccdf, aes(x = Ever60plus_6MTH, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill')

ggsave('C:\\My Proj\\Ever60.png', width = 10, height = 10, units = 'cm')




# 1 dimension: Number of Loans 

ggplot(data = ccdf, aes(x = nloans, fill = Ever30plus_n12MTH)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(0, 75))
ggsave('C:\\My Proj\\nloans.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')

ggplot(data = oddf, aes(x = nloans, fill = Ever30plus_n12MTH)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(0, 75))
ggsave('C:\\My Proj\\nloans-od.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')





# 2 dimensions: Util1 vs MOB1

ggplot(data = ccdf, aes(x = MOB1, y = Utilization1, color = Ever30plus_n12MTH)) +
  geom_point(alpha = 0.3) +
  scale_y_continuous(limits = c(0, 3)) +
  scale_x_continuous(limits = c(0, 500))
ggsave('C:\\My Proj\\MOB_Util1.png', 
       device = 'png', 
       width = 30,
       height = 20,
       units = 'cm')





