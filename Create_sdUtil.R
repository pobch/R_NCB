df$nUtil123 = apply(df[, c('Utilization1', 'Utilization2', 'Utilization3')], 1, function(x) {sum(!is.na(x))}) 
df$stdUtil123 = apply(df[ , c('Utilization1', 'Utilization2', 'Utilization3')], 1, sd, na.rm = T)

df$nUtil456 = apply(df[, c('Utilization4', 'Utilization5', 'Utilization6')], 1, function(x) {sum(!is.na(x))})
df$stdUtil456 = apply(df[ , c('Utilization4', 'Utilization5', 'Utilization6')], 1, sd, na.rm = T)

df$nUtil1to6 = apply(df[, c('Utilization1', 'Utilization2', 'Utilization3', 'Utilization4', 'Utilization5', 'Utilization6')], 1, function(x) {sum(!is.na(x))})
df$stdUtil1to6 = apply(df[ , c('Utilization1', 'Utilization2', 'Utilization3', 'Utilization4', 'Utilization5', 'Utilization6')], 1, sd, na.rm = T)

# Check
table(df$nUtil1to6 == (df$nUtil123 + df$nUtil456))
