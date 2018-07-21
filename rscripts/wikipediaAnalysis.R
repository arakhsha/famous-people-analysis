data = read_csv('data/generated/pantheon_cleaned.csv')
sentimentsData = read_csv('data/generated/wikipedia/sentiments.csv')
data = data %>% left_join(sentimentsData) 

occupationSentiment = data %>% 
  select(occupation, 24:33) %>% 
  group_by(occupation) %>% 
  summarise_all(function(x){mean(x, na.rm = T)})

sentimentTopOccupations = occupationSentiment %>% 
  mutate(occupation = str_to_title(occupation)) %>% 
  gather(key = "sentiment", value = "value", 2:11) %>% 
  group_by(sentiment) %>% 
  arrange(-value) %>% 
  summarise(top1 = occupation[1], top2 = occupation[2], top3 = occupation[3])
saveRDS(sentimentTopOccupations, 'output/sentimentTopOccupations.rds')


data = data %>% 
  mutate(century = ceiling(birthyear / 100))

centuries = min(data$century, na.rm = T):max(data$century, na.rm = T)

timeSentiment = data %>% 
  select(century, 24:33) %>% 
  group_by(century) %>% 
  summarise_all(function(x){mean(x, na.rm = T)}) %>% 
  gather(key = "sentiment", value = "value", 2:11) %>% 
  filter(century< 21)

timeSentimentPlot = hchart(timeSentiment, type = "line",
                           hcaes(x = century, y = value, group = sentiment)) %>% 
  hc_title(text = "Value of Sentiments Over Time")
timeSentimentPlot

timePositive = timeSentiment %>% filter(sentiment == "positive")
timePositivePlot = hchart(timePositive, type = "line",
                           hcaes(x = century, y = value)) %>% 
  hc_title(text = "Value of Positive Sentiments Over Time")
timePositivePlot

saveRDS(timeSentimentPlot, 'output/timeSentimentPlot.rds')
saveRDS(timePositivePlot, 'output/timePositivePlot.rds')
