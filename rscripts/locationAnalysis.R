data = read_csv('data/generated/pantheon_cleaned.csv')
bubbleData = data %>% 
  mutate(century = ceiling(birthyear / 100)) %>% 
  filter(century < 20, !is.na(LAT), !is.na(LON)) %>% 
  select(name, HPI, century, lat = LAT, lon = LON, domain) %>% 
  mutate(HPI = 2^(HPI - min(HPI) + 1))

centuries = min(bubbleData$century, na.rm = T):max(bubbleData$century, na.rm = T)

bubbleData = bubbleData %>% 
  mutate(z = HPI)

sequences = map2(bubbleData$HPI, bubbleData$century, function(hpi, cent){
  ifelse(cent == centuries, hpi, -1)
})
sequences = data_frame(sequence = sequences)

bubbleData = cbind(bubbleData, sequences)


INSTITUTIONS = bubbleData %>% filter(domain == 'INSTITUTIONS')
HUMANITIES = bubbleData %>% filter(domain == 'HUMANITIES')
SCIENCE = bubbleData %>% filter(domain == 'SCIENCE & TECHNOLOGY')
ARTS = bubbleData %>% filter(domain == 'ARTS')
FIGURE = bubbleData %>% filter(domain == 'PUBLIC FIGURE')
LAW = bubbleData %>% filter(domain == 'BUSINESS & LAW')
EXPLORATION = bubbleData %>% filter(domain == 'EXPLORATION')
SPORTS = bubbleData %>% filter(domain == 'SPORTS')

worldBubbleTimePlot = hcmap(download_map_data = F) %>% 
  
  hc_add_series(data = INSTITUTIONS, type = "mapbubble", showInLegend = T, name = 'INSTITUTIONS',
                minSize = 0, maxSize = 10, zThreshold = 0, displayNegative = F) %>% 
  hc_add_series(data = HUMANITIES, type = "mapbubble", showInLegend = T, name = 'HUMANITIES',
                minSize = 0, maxSize = 10, zThreshold = 0, displayNegative = F) %>% 
  hc_add_series(data = SCIENCE, type = "mapbubble", showInLegend = T, name = 'SCIENCE & TECHNOLOGY',
                minSize = 0, maxSize = 10, zThreshold = 0, displayNegative = F) %>% 
  hc_add_series(data = ARTS, type = "mapbubble", showInLegend = T, name = 'ARTS',
                minSize = 0, maxSize = 10, zThreshold = 0, displayNegative = F) %>% 
  hc_add_series(data = FIGURE, type = "mapbubble", showInLegend = T, name = 'PUBLIC FIGURE',
                minSize = 0, maxSize = 10, zThreshold = 0, displayNegative = F) %>% 
  hc_add_series(data = LAW, type = "mapbubble", showInLegend = T, name = 'BUSINESS & LAW',
                minSize = 0, maxSize = 10, zThreshold = 0, displayNegative = F) %>% 
  hc_add_series(data = EXPLORATION, type = "mapbubble", showInLegend = T, name = 'EXPLORATION',
                minSize = 0, maxSize = 10, zThreshold = 0, displayNegative = F) %>% 
  hc_add_series(data = SPORTS, type = "mapbubble", showInLegend = T, name = 'SPORTS',
                minSize = 0, maxSize = 10, zThreshold = 0, displayNegative = F) %>% 
  
  hc_motion(enabled = TRUE, series = 1:8, labels = paste(centuries, 'Century'),
            loop = TRUE, autoPlay = TRUE, axisLabel = 'Century',
            updateInterval = 1000, magnet = list(step =  1)) %>%
  hc_plotOptions(series = list(showInLegend = FALSE)) %>% 
  hc_mapNavigation(enabled = TRUE) %>% 
  hc_legend(enabled = T) %>% 
  hc_tooltip(valueDecimals = 0) %>% 
  hc_title(text = 'Globally Famous People Over Time')
write_rds(worldBubbleTimePlot, 'output/worldBubbleTimePlot.rds')
