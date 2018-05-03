data = read_tsv('data/pantheon/pantheon.tsv')

#creating NA
data[data == 'Unknown' | data == 'Other' | data == 'UNK'] = NA

#birthyear
data$birthyear[data$birthyear == '530s'] = 535
data$birthyear[data$birthyear == '1237?'] = 1237
data$birthyear = as.numeric(data$birthyear)

#birth locations
data = data %>%
  group_by(birthcity) %>% 
  mutate(LAT = ifelse(is.na(LAT) & birthcity != 'Other', first(LAT[!is.na(LAT)]), LAT),
         LON = ifelse(is.na(LON) & birthcity != 'Other', first(LON[!is.na(LON)]), LON)
  ) %>% 
  ungroup()

write_csv(data, 'data/generated/pantheon_cleaned.csv')
