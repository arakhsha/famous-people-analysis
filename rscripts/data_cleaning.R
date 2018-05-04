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
  mutate(LAT = ifelse(is.na(LAT) & !is.na(birthcity), first(LAT[!is.na(LAT)]), LAT),
         LON = ifelse(is.na(LON) & !is.na(birthcity), first(LON[!is.na(LON)]), LON)
  ) %>% 
  ungroup()
locationNAs = data %>% filter(is.na(LAT) | is.na(LON))
missingCitiesLocation = read_csv('data/generated/missingCitiesLocations.csv')
locationNAs = left_join(
  locationNAs %>% select(-LON, -LAT),  
  missingCitiesLocation %>% rename(birthcity = city, LON = lon, LAT = lat),
  by = 'birthcity'
)
data = rbind(
    data %>% filter(!is.na(LON), !is.na(LAT)),
    locationNAs
  ) %>% 
  arrange(name)
write_csv(data, 'data/generated/pantheon_cleaned.csv')
