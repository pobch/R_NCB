
# CC

ggplot(data = ccdf, aes(x = All_0_6Mth, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill')

ggplot(data = ccdf, aes(x = ALL_0_below6Mth, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill')


# PL revolve

ggplot(data = plrevo, aes(x = All_0_6Mth, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill')

ggplot(data = plrevo, aes(x = ALL_0_below6Mth, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill')


# PL Non revolve

ggplot(data = plnonrevo, aes(x = All_0_6Mth, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill')

ggplot(data = plnonrevo, aes(x = ALL_0_below6Mth, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill')
