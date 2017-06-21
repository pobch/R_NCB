# Plot MOB1

ggplot(data = plrevo, aes(x = MOB1, fill = Ever30plus_n12MTH)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0,200,6))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\PL_revo-MOB1.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')
ggplot(data = plrevo, aes(x = MOB1, fill = Ever30plus_n12MTH)) +
  geom_histogram(binwidth = 1, position = 'fill') +
  scale_x_continuous(breaks = seq(0,200,6))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\PL_revo-MOB1_fill.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')

ggplot(data = plnonrevo, aes(x = MOB1, fill = Ever30plus_n12MTH)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0,200,6))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\PL_nonrevo-MOB1.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')
ggplot(data = plnonrevo, aes(x = MOB1, fill = Ever30plus_n12MTH)) +
  geom_histogram(binwidth = 1, position = 'fill') +
  scale_x_continuous(breaks = seq(0,200,6))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\PL_nonrevo-MOB1_fill.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')

ggplot(data = subset(ccdf, MOB1 != 1388), aes(x = MOB1, fill = Ever30plus_n12MTH)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0,200,6))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\CC-MOB1.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')
ggplot(data = subset(ccdf, MOB1 != 1388), aes(x = MOB1, fill = Ever30plus_n12MTH)) +
  geom_histogram(binwidth = 1, position = 'fill') +
  scale_x_continuous(breaks = seq(0,200,6))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\CC-MOB1_fill.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')

ggplot(data = subset(oddf, MOB1 < 300), aes(x = MOB1, fill = Ever30plus_n12MTH)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0,200,6))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\OD-MOB1.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')
ggplot(data = subset(oddf, MOB1 < 300), aes(x = MOB1, fill = Ever30plus_n12MTH)) +
  geom_histogram(binwidth = 1, position = 'fill') +
  scale_x_continuous(breaks = seq(0,200,6))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\OD-MOB1_fill.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')

