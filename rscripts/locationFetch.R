library(stringr)
library(ggmap) 
data = read_tsv('data/pantheon/pantheon.tsv')
data[data == 'Unknown' | data == 'Other' | data == 'UNK' | data == 'Missing'] = NA
locationNAs = data %>% filter(is.na(LAT) | is.na(LON))

missingCities = sort(unique(locationNAs$birthcity))
missingCities = missingCities[!is.na(missingCities)]

missingCitiesLocations = data.frame(city = character(), lon = numeric(), lat = numeric())
for(city in missingCities[51:171]) {
  print(which(missingCities == city))
  try = TRUE;
  while(try) {
    tryCatch(
      {
        result = geocode(city, force = T)
        try = FALSE
      },
      warning = function(war) {
        if(str_detect(war, 'OVER_QUERY_LIMIT')) {
          print(war)
          try <<- TRUE
        }
        else {
          print(war)
          try <<- FALSE
          result <<- data.frame(lon = NA, lat = NA)
        }
      })
  }
  row = cbind(as.data.frame(city), result)
  print(row)
  missingCitiesLocations = rbind(missingCitiesLocations, row)
}

write_csv(missingCitiesLocations, 'data/generated/missingCitiesLocations.csv')
