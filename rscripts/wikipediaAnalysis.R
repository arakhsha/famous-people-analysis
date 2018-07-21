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

industrySentiment = data %>% 
  select(industry, 24:33, -positive, -negative) %>% 
  group_by(industry) %>% 
  summarise_all(function(x){mean(x, na.rm = T)}) %>% 
  gather(key = "sentiment", value = "value", 2:9)

industrySentimentPlot = hchart(industrySentiment, hcaes(x = industry, y = value, group = sentiment), type = "bar") %>% 
  hc_plotOptions(bar = list(stacking = "percent"))%>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_yAxis(title = list(text = "")) %>% 
  hc_title(text = "Industries Sentiments")
industrySentimentPlot
saveRDS(industrySentimentPlot, 'output/industrySentimentPlot.rds')

industryPositive = data %>% 
  select(industry, positive, negative) %>% 
  group_by(industry) %>% 
  summarise_all(function(x){mean(x, na.rm = T)}) %>% 
  arrange(positive / negative) %>% 
  gather(key = "sentiment", value = "value", 2:3)

industryPositivePlot = hchart(industryPositive, hcaes(x = industry, y = value, group = sentiment), type = "bar") %>% 
  hc_plotOptions(bar = list(stacking = "percent")) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_yAxis(title = list(text = "")) %>% 
  hc_title(text = "Industries Positiveness")
industryPositivePlot
saveRDS(industryPositivePlot, "output/industryPositivePlot.rds")

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
