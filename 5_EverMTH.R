
# CC

sixmths = ccdf %>% gather(key = 'ever6mth', value = 'val', EverXplus_6MTH, Ever30plus_6MTH, Ever60plus_6MTH)
ggplot(data = sixmths, aes(x = val, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill') +
  facet_grid(. ~ ever6mth)


threemths = ccdf %>% gather(key = 'ever3mth', value = 'val', EverXplus_3MTH, Ever30plus_3MTH, Ever60plus_3MTH)
ggplot(data = threemths, aes(x = val, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill') +
  facet_grid(. ~ ever3mth)


# PL Revolve

sixmths = plrevo %>% gather(key = 'ever6mth', value = 'val', EverXplus_6MTH, Ever30plus_6MTH, Ever60plus_6MTH)
ggplot(data = sixmths, aes(x = val, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill') +
  facet_grid(. ~ ever6mth)


threemths = plrevo %>% gather(key = 'ever3mth', value = 'val', EverXplus_3MTH, Ever30plus_3MTH, Ever60plus_3MTH)
ggplot(data = threemths, aes(x = val, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill') +
  facet_grid(. ~ ever3mth)


# PL non Revolve

sixmths = plnonrevo %>% gather(key = 'ever6mth', value = 'val', EverXplus_6MTH, Ever30plus_6MTH, Ever60plus_6MTH)
ggplot(data = sixmths, aes(x = val, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill') +
  facet_grid(. ~ ever6mth)


threemths = plnonrevo %>% gather(key = 'ever3mth', value = 'val', EverXplus_3MTH, Ever30plus_3MTH, Ever60plus_3MTH)
ggplot(data = threemths, aes(x = val, fill = Ever30plus_n12MTH)) +
  geom_bar(position = 'fill') +
  facet_grid(. ~ ever3mth)


# OD doesnt have Ever_MTH variables

