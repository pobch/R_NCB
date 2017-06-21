

# utilbreak = c(-Inf, seq(0,1.5,0.01), seq(1.6,5,0.1), 1000000000)
utilbreak = c(-Inf, seq(0,1.5,0.02), 1000000000)

# Percent Ever30plus_n12MTH = Y in each chunk 
tblPercent = function(x, y) {
  interval = cut(x, breaks = utilbreak)
  percentY = as.data.frame.matrix(table(interval, y, useNA = 'always'))
  rownames(percentY)[is.na(rownames(percentY))] = 'NA'
  colnames(percentY)[is.na(colnames(percentY))] = 'NA'
  percentY$percentY = round(percentY$Y * 100 / (percentY$N + percentY$Y), 2)
  percentY
}
# Percent of obs in each chunk
tblFun <- function(x){
  interval = cut(x, breaks = utilbreak)
  tbl <- table(interval, useNA = 'always')
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  res = as.data.frame.matrix(res)
  colnames(res) <- c('Count','Percentage')
  rownames(res)[is.na(rownames(res))] = 'NA'
  res
}


# CC
PercentU123 = cbind(tblPercent(ccdf$Utilization1, ccdf$Ever30plus_n12MTH), 
                    tblPercent(ccdf$Utilization2, ccdf$Ever30plus_n12MTH), 
                    tblPercent(ccdf$Utilization3, ccdf$Ever30plus_n12MTH))
colnames(PercentU123) = c('N1', 'Y1', 'NA1', 'PercentY_u1', 
                          'N2', 'Y2', 'NA2', 'PercentY_u2', 
                          'N3', 'Y3', 'NA3', 'PercentY_u3')
PercentU123 = rownames_to_column(PercentU123, 'chunk')
PercentU123$chunk = factor(PercentU123$chunk, levels = PercentU123$chunk)


percentObs = cbind(tblFun(ccdf$Utilization1),
                   tblFun(ccdf$Utilization2),
                   tblFun(ccdf$Utilization3))
colnames(percentObs) = c('Count_u1', 'Percent_u1', 'Count_u2', 'Percent_u2', 'Count_u3', 'Percent_u3')
percentObs = rownames_to_column(percentObs, 'chunk')
percentObs$chunk = factor(percentObs$chunk, levels = percentObs$chunk)


ggplot(data = PercentU123, aes(x = chunk)) +
  geom_point(aes(y = PercentY_u1), color = 'red', alpha = 0.5) +
  geom_line(aes(y = PercentY_u1, group = 1), color = 'red', alpha = 0.5) +
  geom_point(aes(y = PercentY_u2), color = 'blue', alpha = 0.5) +
  geom_line(aes(y = PercentY_u2, group = 1), color = 'blue', alpha = 0.5) +
  geom_point(aes(y = PercentY_u3), color = 'green', alpha = 0.5) +
  geom_line(aes(y = PercentY_u3, group = 1), color = 'green', alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.2))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\CC-Util123-PercentY.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')
ggplot(data = percentObs, aes(x = chunk)) +
  geom_bar(aes(y = Percent_u1), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.2))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\CC-Util1-PercentObs.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')


# PL revolve

PercentU123 = cbind(tblPercent(plrevo$Utilization1, plrevo$Ever30plus_n12MTH), 
                    tblPercent(plrevo$Utilization2, plrevo$Ever30plus_n12MTH), 
                    tblPercent(plrevo$Utilization3, plrevo$Ever30plus_n12MTH))
colnames(PercentU123) = c('N1', 'Y1', 'NA1', 'PercentY_u1', 
                          'N2', 'Y2', 'NA2', 'PercentY_u2', 
                          'N3', 'Y3', 'NA3', 'PercentY_u3')
PercentU123 = rownames_to_column(PercentU123, 'chunk')
PercentU123$chunk = factor(PercentU123$chunk, levels = PercentU123$chunk)

percentObs = cbind(tblFun(plrevo$Utilization1),
                   tblFun(plrevo$Utilization2),
                   tblFun(plrevo$Utilization3))
colnames(percentObs) = c('Count_u1', 'Percent_u1', 'Count_u2', 'Percent_u2', 'Count_u3', 'Percent_u3')
percentObs = rownames_to_column(percentObs, 'chunk')
percentObs$chunk = factor(percentObs$chunk, levels = percentObs$chunk)

ggplot(data = PercentU123, aes(x = chunk)) +
  geom_point(aes(y = PercentY_u1), color = 'red', alpha = 0.5) +
  geom_line(aes(y = PercentY_u1, group = 1), color = 'red', alpha = 0.5) +
  geom_point(aes(y = PercentY_u2), color = 'blue', alpha = 0.5) +
  geom_line(aes(y = PercentY_u2, group = 1), color = 'blue', alpha = 0.5) +
  geom_point(aes(y = PercentY_u3), color = 'green', alpha = 0.5) +
  geom_line(aes(y = PercentY_u3, group = 1), color = 'green', alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.2))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\PL_revo-Util123-PercentY.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')
ggplot(data = percentObs, aes(x = chunk)) +
  geom_bar(aes(y = Percent_u1), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.2))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\PL_revo-Util1-PercentObs.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')

# PL non-revolve

PercentU123 = cbind(tblPercent(plnonrevo$Utilization1, plnonrevo$Ever30plus_n12MTH), 
                    tblPercent(plnonrevo$Utilization2, plnonrevo$Ever30plus_n12MTH), 
                    tblPercent(plnonrevo$Utilization3, plnonrevo$Ever30plus_n12MTH))
colnames(PercentU123) = c('N1', 'Y1', 'NA1', 'PercentY_u1', 
                          'N2', 'Y2', 'NA2', 'PercentY_u2', 
                          'N3', 'Y3', 'NA3', 'PercentY_u3')
PercentU123 = rownames_to_column(PercentU123, 'chunk')
PercentU123$chunk = factor(PercentU123$chunk, levels = PercentU123$chunk)

percentObs = cbind(tblFun(plnonrevo$Utilization1),
                   tblFun(plnonrevo$Utilization2),
                   tblFun(plnonrevo$Utilization3))
colnames(percentObs) = c('Count_u1', 'Percent_u1', 'Count_u2', 'Percent_u2', 'Count_u3', 'Percent_u3')
percentObs = rownames_to_column(percentObs, 'chunk')
percentObs$chunk = factor(percentObs$chunk, levels = percentObs$chunk)

ggplot(data = PercentU123, aes(x = chunk)) +
  geom_point(aes(y = PercentY_u1), color = 'red', alpha = 0.5) +
  geom_line(aes(y = PercentY_u1, group = 1), color = 'red', alpha = 0.5) +
  geom_point(aes(y = PercentY_u2), color = 'blue', alpha = 0.5) +
  geom_line(aes(y = PercentY_u2, group = 1), color = 'blue', alpha = 0.5) +
  geom_point(aes(y = PercentY_u3), color = 'green', alpha = 0.5) +
  geom_line(aes(y = PercentY_u3, group = 1), color = 'green', alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.2))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\PL_nonrevo-Util123-PercentY.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')
ggplot(data = percentObs, aes(x = chunk)) +
  geom_bar(aes(y = Percent_u1), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.2))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\PL_nonrevo-Util1-PercentObs.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')


# OD

PercentU123 = cbind(tblPercent(oddf$Utilization1, oddf$Ever30plus_n12MTH), 
                    tblPercent(oddf$Utilization2, oddf$Ever30plus_n12MTH), 
                    tblPercent(oddf$Utilization3, oddf$Ever30plus_n12MTH))
colnames(PercentU123) = c('N1', 'Y1', 'NA1', 'PercentY_u1', 
                          'N2', 'Y2', 'NA2', 'PercentY_u2', 
                          'N3', 'Y3', 'NA3', 'PercentY_u3')
PercentU123 = rownames_to_column(PercentU123, 'chunk')
PercentU123$chunk = factor(PercentU123$chunk, levels = PercentU123$chunk)

percentObs = cbind(tblFun(oddf$Utilization1),
                   tblFun(oddf$Utilization2),
                   tblFun(oddf$Utilization3))
colnames(percentObs) = c('Count_u1', 'Percent_u1', 'Count_u2', 'Percent_u2', 'Count_u3', 'Percent_u3')
percentObs = rownames_to_column(percentObs, 'chunk')
percentObs$chunk = factor(percentObs$chunk, levels = percentObs$chunk)

ggplot(data = PercentU123, aes(x = chunk)) +
  geom_point(aes(y = PercentY_u1), color = 'red', alpha = 0.5) +
  geom_line(aes(y = PercentY_u1, group = 1), color = 'red', alpha = 0.5) +
  geom_point(aes(y = PercentY_u2), color = 'blue', alpha = 0.5) +
  geom_line(aes(y = PercentY_u2, group = 1), color = 'blue', alpha = 0.5) +
  geom_point(aes(y = PercentY_u3), color = 'green', alpha = 0.5) +
  geom_line(aes(y = PercentY_u3, group = 1), color = 'green', alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.2))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\OD-Util123-PercentY.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')
ggplot(data = percentObs, aes(x = chunk)) +
  geom_bar(aes(y = Percent_u1), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.2))
ggsave('D:\\SAS_data\\Chk_data\\Pics\\OD-Util1-PercentObs.png', 
       device = 'png', 
       width = 50,
       height = 10,
       units = 'cm')
