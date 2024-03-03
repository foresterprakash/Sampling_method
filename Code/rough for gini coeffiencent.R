dat_gini <- select(dat_analysed,"plot_id","BA_sqm")

dat_gini <- dat_gini %>%
  group_by(plot_id)%>%
  arrange(plot_id,BA_sqm)%>%
  mutate(n = n()) %>%
  mutate(j = 1:n()) %>%
  ungroup() %>%
  mutate(numerator = (2 * j - n- 1)* BA_sqm) %>%
  mutate(denominator = BA_sqm*(n-1))

dat_gini_sum <- dat_gini %>%
  group_by(plot_id) %>%
  summarise(numerator = sum(numerator),denominator = sum(denominator)) %>%
  ungroup() %>%
  mutate(gini_coefficient = numerator /denominator)


