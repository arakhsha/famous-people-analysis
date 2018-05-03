library(stringr)

data = read_tsv('data/pantheon/pantheon.tsv')
data[data == 'Unknown' | data == 'Other' | data == 'UNK' | data == 'Missing'] = NA
locationNAs = data %>% filter(is.na(LAT) | is.na(LON))

missingCities = sort(unique(locationNAs$birthcity))
missingCities = missingCities[!is.na(missingCities)]

missingCitiesLocations = data.frame(city = character(), lon = numeric(), lat = numeric())
for(city in missingCities) {
  print(city)
  try = TRUE;
  while(try)
  tryCatch({
    result = geocode(city)
    try = FALSE
    },
    warning = function(war) {
      if(str_detect(war, 'OVER_QUERY_LIMIT')) {
        print(war)
        print('failed!')
      }
      else {
        print(war)
        try <<- FALSE
        result <<- data.frame(lon = NA, lat = NA)
      }
    })
  row = cbind(as.data.frame(city), result)
  missingCitiesLocations = rbind(missingCitiesLocations, row)
}

write_csv(missingCitiesLocations, 'data/generated/missingCitiesLocations.csv')
