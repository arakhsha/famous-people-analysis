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